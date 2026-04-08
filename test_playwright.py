# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "playwright",
#     "pytest",
#     "pytest-playwright",
#     "Pillow",
# ]
# ///
"""
Playwright structural tests and Docker-based visual regression tests for Illuminate SVG output.

Structural tests use Playwright (headless Chromium) to inspect SVG DOM elements.
Visual regression tests render SVGs via Inkscape inside a Docker container
(visual_tests/Dockerfile) with pinned fonts to guarantee identical pixel
output across macOS and Linux.

Run with:
    uv run test_playwright.py

Or for pytest mode:
    uv run pytest test_playwright.py -v

To update expected baselines:
    UPDATE_BASELINES=1 uv run test_playwright.py
"""
import os
import shutil
import subprocess
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# Ensure Playwright browsers are installed
# ---------------------------------------------------------------------------

def ensure_browsers():
    """Install Playwright Chromium if not already present."""
    try:
        from playwright.sync_api import sync_playwright
        with sync_playwright() as p:
            p.chromium.launch(headless=True).close()
    except Exception:
        subprocess.run(
            [sys.executable, "-m", "playwright", "install", "chromium"],
            check=True,
        )

# ---------------------------------------------------------------------------
# Generate reference SVGs by running lake test (which writes smiley.svg, commdiag.svg)
# ---------------------------------------------------------------------------

ROOT = Path(__file__).resolve().parent
VISUAL_DIR = ROOT / "visual_tests"
UPDATE_BASELINES = os.environ.get("UPDATE_BASELINES", "").lower() in ("1", "true", "yes")

DOCKER_IMAGE = "illuminate-inkscape"


def generate_svgs():
    """Run lake test to generate reference SVGs."""
    result = subprocess.run(
        ["lake", "test"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=300,
    )
    if result.returncode != 0:
        print("lake test stderr:", result.stderr, file=sys.stderr)
        raise RuntimeError(f"lake test failed:\n{result.stdout}")
    return result.stdout


# ---------------------------------------------------------------------------
# Docker-based rsvg-convert visual test helpers
# ---------------------------------------------------------------------------

def _ensure_docker_image():
    """Build the rsvg-convert Docker image if it doesn't exist."""
    # Check if image exists
    result = subprocess.run(
        ["docker", "image", "inspect", DOCKER_IMAGE],
        capture_output=True,
    )
    if result.returncode == 0:
        return
    print("  Building Docker image for visual tests...")
    result = subprocess.run(
        ["docker", "build", "-t", DOCKER_IMAGE, str(VISUAL_DIR)],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise RuntimeError(f"Docker build failed:\n{result.stderr}")


def _render_svg_to_png(svg_name: str) -> bytes:
    """Render an SVG to PNG using Inkscape in Docker."""
    svg_path = ROOT / svg_name
    assert svg_path.exists(), f"{svg_name} not found — run lake test first"
    svg_data = svg_path.read_bytes()
    result = subprocess.run(
        ["docker", "run", "--rm", "-i", DOCKER_IMAGE],
        input=svg_data,
        capture_output=True,
    )
    assert result.returncode == 0, f"inkscape failed: {result.stderr.decode()}"
    png_data = result.stdout
    assert len(png_data) > 100, f"inkscape produced empty output for {svg_name}"
    return png_data


def pixel_diff_ratio(img1_bytes: bytes, img2_bytes: bytes) -> float:
    """Return fraction of pixels that differ between two images."""
    from PIL import Image
    import io

    im1 = Image.open(io.BytesIO(img1_bytes)).convert("RGBA")
    im2 = Image.open(io.BytesIO(img2_bytes)).convert("RGBA")

    if im1.size != im2.size:
        return 1.0  # completely different

    b1 = im1.tobytes()
    b2 = im2.tobytes()
    channels = 4  # RGBA
    total = len(b1) // channels
    diff = sum(
        1 for i in range(total)
        if b1[i*channels:(i+1)*channels] != b2[i*channels:(i+1)*channels]
    )
    return diff / total


def _run_visual_test(svg_name: str, test_name: str):
    """Render SVG via Docker rsvg-convert, write actual PNG, compare against expected."""
    VISUAL_DIR.mkdir(exist_ok=True)
    _ensure_docker_image()
    rendered = _render_svg_to_png(svg_name)

    actual_path = VISUAL_DIR / f"{test_name}.actual.png"
    expected_path = VISUAL_DIR / f"{test_name}.expected.png"

    actual_path.write_bytes(rendered)

    if UPDATE_BASELINES:
        expected_path.write_bytes(rendered)

    if not expected_path.exists():
        raise AssertionError(
            f"No expected baseline at {expected_path.relative_to(ROOT)}. "
            f"Run with UPDATE_BASELINES=1 to create it."
        )

    baseline = expected_path.read_bytes()
    ratio = pixel_diff_ratio(rendered, baseline)
    assert ratio < 0.001, (
        f"{test_name} visual regression: {ratio:.4%} pixels differ. "
        f"Compare {actual_path.name} vs {expected_path.name} in visual_tests/"
    )


# ---------------------------------------------------------------------------
# Structural tests (Playwright)
# ---------------------------------------------------------------------------

def test_smiley_structure(page):
    """Smiley SVG has expected element counts and structure."""
    svg_path = ROOT / "smiley.svg"
    assert svg_path.exists(), "smiley.svg not found — run lake test first"
    page.goto(f"file://{svg_path}")

    # Should have path elements (face, eyes, smile)
    paths = page.locator("path")
    count = paths.count()
    assert count >= 4, f"Expected >= 4 <path> elements, got {count}"

    # Should have a yellow fill (face)
    svg_content = svg_path.read_text()
    assert "rgb(255,220,50)" in svg_content, "Missing yellow face color"

    # Should have transform groups (for eye positioning)
    groups = page.locator("g[transform]")
    g_count = groups.count()
    assert g_count >= 1, f"Expected >= 1 <g transform> elements, got {g_count}"


def test_smiley_bounding_box(page):
    """Smiley SVG renders with reasonable bounding box dimensions."""
    svg_path = ROOT / "smiley.svg"
    page.goto(f"file://{svg_path}")

    bbox = page.locator("svg").bounding_box()
    assert bbox is not None, "SVG element not found"
    assert bbox["width"] > 50, f"SVG too narrow: {bbox['width']}"
    assert bbox["height"] > 50, f"SVG too short: {bbox['height']}"


def test_commdiag_structure(page):
    """Commutative diagram SVG has nodes and arrows."""
    svg_path = ROOT / "commdiag.svg"
    assert svg_path.exists(), "commdiag.svg not found — run lake test first"
    page.goto(f"file://{svg_path}")

    # Should have text elements for node labels (A, B, C, D)
    texts = page.locator("text")
    text_count = texts.count()
    assert text_count >= 4, f"Expected >= 4 <text> elements, got {text_count}"

    # Should have path elements for arrows
    paths = page.locator("path")
    path_count = paths.count()
    assert path_count >= 4, f"Expected >= 4 <path> elements (arrows), got {path_count}"


def test_commdiag_labels(page):
    """Commutative diagram has the expected label text content."""
    svg_path = ROOT / "commdiag.svg"
    page.goto(f"file://{svg_path}")

    svg_content = svg_path.read_text()
    for label in ["A", "B", "C", "D", "f", "g", "h", "k"]:
        assert f">{label}</text>" in svg_content, f"Missing label '{label}'"


def test_commdiag_annotations(page):
    """Commutative diagram SVG does not crash with annotations."""
    svg_path = ROOT / "commdiag.svg"
    page.goto(f"file://{svg_path}")

    # Just verify the SVG renders without errors
    svg = page.locator("svg")
    assert svg.count() == 1, "Expected exactly one SVG element"


def test_commdiag_node_positions(page):
    """Nodes A and B should be horizontally separated (A left of B)."""
    svg_path = ROOT / "commdiag.svg"
    page.goto(f"file://{svg_path}")

    # Get bounding boxes of text elements
    texts = page.locator("text")
    text_count = texts.count()

    positions = []
    for i in range(text_count):
        bbox = texts.nth(i).bounding_box()
        content = texts.nth(i).text_content()
        if bbox:
            positions.append((content, bbox))

    # Find A and B positions
    a_pos = next((p for c, p in positions if c == "A"), None)
    b_pos = next((p for c, p in positions if c == "B"), None)

    if a_pos and b_pos:
        # A should be to the left of B (in SVG coordinates)
        assert a_pos["x"] < b_pos["x"], (
            f"A (x={a_pos['x']}) should be left of B (x={b_pos['x']})"
        )


def test_stars_structure(page):
    """Stars SVG has path elements for each star and uses dash patterns."""
    svg_path = ROOT / "stars.svg"
    assert svg_path.exists(), "stars.svg not found — run lake test first"
    page.goto(f"file://{svg_path}")

    # Should have many path elements (fill + stroke for each star)
    paths = page.locator("path")
    count = paths.count()
    assert count >= 20, f"Expected >= 20 <path> elements, got {count}"

    # Should have dash patterns
    svg_content = svg_path.read_text()
    assert "stroke-dasharray" in svg_content, "Missing stroke-dasharray for dashed stars"


def test_cellophane_clip_structure(page):
    """Cellophane/clip SVG has opacity groups and clipPath elements."""
    svg_path = ROOT / "cellophane-clip.svg"
    assert svg_path.exists(), "cellophane-clip.svg not found — run lake test first"
    svg_content = svg_path.read_text()
    assert "opacity" in svg_content, "Missing opacity attribute for cellophane"
    assert "clipPath" in svg_content, "Missing clipPath element for clip"


# ---------------------------------------------------------------------------
# Visual regression tests (Docker rsvg-convert)
# ---------------------------------------------------------------------------

def test_smiley_visual():
    """Compare smiley rendering against expected baseline."""
    _run_visual_test("smiley.svg", "smiley")


def test_commdiag_visual():
    """Compare commdiag rendering against expected baseline."""
    _run_visual_test("commdiag.svg", "commdiag")


def test_roundedrects_visual():
    """Compare rounded-rects rendering against expected baseline."""
    _run_visual_test("roundedrects.svg", "roundedrects")

def test_roundedrects_2_5_visual():
    """Compare rounded-rects rendering against expected baseline."""
    _run_visual_test("roundedrects_2_5.svg", "roundedrects_2_5")

def test_roundedrects_2_10_visual():
    """Compare rounded-rects rendering against expected baseline."""
    _run_visual_test("roundedrects_2_10.svg", "roundedrects_2_10")

def test_roundedrects_7_2_visual():
    """Compare rounded-rects rendering against expected baseline."""
    _run_visual_test("roundedrects_7_2.svg", "roundedrects_7_2")

def test_roundedrects_7_5_visual():
    """Compare rounded-rects rendering against expected baseline."""
    _run_visual_test("roundedrects_7_5.svg", "roundedrects_7_5")


def test_pipeline_visual():
    """Compare pipeline diagram rendering against expected baseline."""
    _run_visual_test("pipeline.svg", "pipeline")


def test_stringlayout_visual():
    """Compare string layout diagram rendering against expected baseline."""
    _run_visual_test("string-layout.svg", "stringlayout")


def test_coechain_visual():
    """Compare coe-chain diagram rendering against expected baseline."""
    _run_visual_test("coe-chain.svg", "coechain")


def test_lakeworkspace_visual():
    """Compare lake workspace diagram rendering against expected baseline."""
    _run_visual_test("lake-workspace.svg", "lakeworkspace")


def test_stars_visual():
    """Compare stars rendering against expected baseline."""
    _run_visual_test("stars.svg", "stars")


def test_star_anchors_visual():
    """Compare star-anchors rendering against expected baseline."""
    _run_visual_test("star-anchors.svg", "star-anchors")


def test_ellipse_visual():
    """Compare ellipse rendering against expected baseline."""
    _run_visual_test("ellipse.svg", "ellipse")


def test_transforms_visual():
    """Compare transforms rendering against expected baseline."""
    _run_visual_test("transforms.svg", "transforms")


def test_ghost_refocus_visual():
    """Compare ghost-refocus rendering against expected baseline."""
    _run_visual_test("ghost-refocus.svg", "ghost-refocus")


def test_cellophane_clip_visual():
    """Compare cellophane-clip rendering against expected baseline."""
    _run_visual_test("cellophane-clip.svg", "cellophane-clip")


def test_trace_connect_angled_visual():
    """Compare trace-connect-angled rendering against expected baseline."""
    _run_visual_test("trace-connect-angled.svg", "trace-connect-angled")


def test_pizza_visual():
    """Compare pizza wedge rendering against expected baseline."""
    _run_visual_test("pizza.svg", "pizza")


def test_filmstrip_growing_circle_visual():
    """Compare filmstrip growing-circle rendering against expected baseline."""
    _run_visual_test("filmstrip-growing-circle.svg", "filmstrip-growing-circle")


def test_filmstrip_rotating_square_visual():
    """Compare filmstrip rotating-square rendering against expected baseline."""
    _run_visual_test("filmstrip-rotating-square.svg", "filmstrip-rotating-square")


def test_filmstrip_animated_clip_visual():
    """Compare filmstrip animated-clip rendering against expected baseline."""
    _run_visual_test("filmstrip-animated-clip.svg", "filmstrip-animated-clip")


def test_filmstrip_morph_rect_circle_visual():
    """Compare filmstrip morph rect-to-circle rendering against expected baseline."""
    _run_visual_test("filmstrip-morph-rect-circle.svg", "filmstrip-morph-rect-circle")


def test_filmstrip_morph_nested_visual():
    """Compare filmstrip morph nested named diagrams rendering against expected baseline."""
    _run_visual_test("filmstrip-morph-nested.svg", "filmstrip-morph-nested")


def test_filmstrip_morph_arrows_visual():
    """Compare filmstrip morph arrow tests rendering against expected baseline."""
    _run_visual_test("filmstrip-morph-arrows.svg", "filmstrip-morph-arrows")



# ---------------------------------------------------------------------------
# Standalone animation player tests
# ---------------------------------------------------------------------------

def test_standalone_player_seek_updates_content(page):
    """Scrubbing the standalone player to a different frame should change the displayed SVG.

    The standalone playerJs renders the sync frame SVG then patches varying attributes
    via paramMap/params. Seeking to a later frame should update the displayed content.
    """
    html_path = ROOT / "test_output" / "anim-seek-test.html"
    assert html_path.exists(), "anim-seek-test.html not found — run lake test first"
    page.goto(f"file://{html_path}")

    # Get the SVG content at frame 0
    frame0_svg = page.evaluate("document.getElementById('anim-container').innerHTML")
    assert "<svg" in frame0_svg, "Container should have SVG at frame 0"

    # Seek to the last frame via the scrubber
    last_frame = page.evaluate("parseInt(document.getElementById('anim-scrub').max)")
    page.evaluate(f"""
        var scrubber = document.getElementById('anim-scrub');
        scrubber.value = {last_frame};
        scrubber.dispatchEvent(new Event('input'));
    """)

    # Get the SVG content at the last frame
    last_frame_svg = page.evaluate("document.getElementById('anim-container').innerHTML")
    assert "<svg" in last_frame_svg, "Container should have SVG at last frame"

    # The circle grows from radius 10 to ~50, so the path data MUST differ.
    # If they're equal, the player is showing stale sync-frame content.
    assert frame0_svg != last_frame_svg, (
        "Seeking to the last frame should change the displayed SVG "
        "(circle grows from r=10 to r≈50), but the content is identical — "
        "the standalone player is not applying per-frame updates"
    )


def test_standalone_player_loop_does_not_stop(page):
    """A single looping step should never reach 'stopped' state.

    The standalone playerJs tick() function does not handle the loop flag on
    steps. When frame >= totalFrames, it unconditionally sets playing = false
    and shows the play button, even though a looping step should wrap around.
    """
    html_path = ROOT / "test_output" / "anim-loop-test.html"
    assert html_path.exists(), "anim-loop-test.html not found — run lake test first"
    page.goto(f"file://{html_path}")

    # Click play to start the animation
    page.click("#anim-play")

    # Wait long enough for the animation to have played through at least once
    # (the animation is 1 second at 10fps = 10 frames)
    page.wait_for_timeout(1500)

    # If loop handling works, the animation should still be playing.
    # The play button text should be the pause icon (⏸ = \u23F8), not play (▶ = \u25B6).
    btn_text = page.evaluate("document.getElementById('anim-play').textContent")
    assert btn_text == "\u23F8", (
        f"After 1.5s, a 1s looping animation should still be playing "
        f"(button should show ⏸), but button shows '{btn_text}' — "
        f"the standalone player does not handle looping steps"
    )


def test_standalone_player_dual_animation_independence(page):
    """Two animations embedded in the same page should each render into their own container.

    Each renderRevealHTML call targets a different CSS selector, so both
    animations should render independently without clobbering each other.
    """
    html_path = ROOT / "test_output" / "anim-dual-test.html"
    assert html_path.exists(), "anim-dual-test.html not found — run lake test first"
    page.goto(f"file://{html_path}")

    anim_a_svg = page.evaluate("document.querySelector('#anim-a')?.innerHTML || ''")
    anim_b_svg = page.evaluate("document.querySelector('#anim-b')?.innerHTML || ''")

    # Both containers should have SVG content
    assert "<svg" in anim_a_svg, "Animation A container should have SVG content"
    assert "<svg" in anim_b_svg, "Animation B container should have SVG content"

    # Animation A is a red circle, animation B is a blue rectangle.
    # They should have DIFFERENT content because they're different animations.
    assert anim_a_svg != anim_b_svg, (
        "Two different animations (red circle vs blue rectangle) should show "
        "different SVG content, but they are identical — one animation's "
        "data may have clobbered the other"
    )


# ---------------------------------------------------------------------------
# Direct runner (not pytest)
# ---------------------------------------------------------------------------

def main():
    """Run all tests directly without pytest."""
    print("Generating SVGs via lake test...")
    output = generate_svgs()
    for line in output.strip().split("\n")[-3:]:
        print(f"  {line}")

    print("\nInstalling Playwright browsers if needed...")
    ensure_browsers()

    from playwright.sync_api import sync_playwright

    structural_tests = [
        test_smiley_structure,
        test_smiley_bounding_box,
        test_commdiag_structure,
        test_commdiag_labels,
        test_commdiag_annotations,
        test_commdiag_node_positions,
        test_stars_structure,
        test_cellophane_clip_structure,
        test_standalone_player_seek_updates_content,
        test_standalone_player_loop_does_not_stop,
        test_standalone_player_dual_animation_independence,
    ]

    visual_tests = [
        test_smiley_visual,
        test_commdiag_visual,
        test_roundedrects_visual,
        test_roundedrects_2_5_visual,
        test_roundedrects_2_10_visual,
        test_roundedrects_7_2_visual,
        test_roundedrects_7_5_visual,
        test_pipeline_visual,
        test_stringlayout_visual,
        test_coechain_visual,
        test_lakeworkspace_visual,
        test_stars_visual,
        test_star_anchors_visual,
        test_ellipse_visual,
        test_transforms_visual,
        test_ghost_refocus_visual,
        test_cellophane_clip_visual,
        test_trace_connect_angled_visual,
        test_pizza_visual,
        test_filmstrip_growing_circle_visual,
        test_filmstrip_rotating_square_visual,
        test_filmstrip_animated_clip_visual,
        test_filmstrip_morph_rect_circle_visual,
        test_filmstrip_morph_nested_visual,
        test_filmstrip_morph_arrows_visual,
    ]

    passed = 0
    failed = 0

    # Run structural tests with Playwright
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)

        for test_fn in structural_tests:
            name = test_fn.__name__
            page = browser.new_page()
            try:
                test_fn(page)
                print(f"  ✓ {name}")
                passed += 1
            except Exception as e:
                print(f"  ✗ {name}: {e}")
                failed += 1
            finally:
                page.close()

        browser.close()

    # Run visual regression tests with Docker rsvg-convert
    for test_fn in visual_tests:
        name = test_fn.__name__
        try:
            test_fn()
            print(f"  ✓ {name}")
            passed += 1
        except Exception as e:
            print(f"  ✗ {name}: {e}")
            failed += 1

    print(f"\n{passed} passed, {failed} failed")
    if failed > 0:
        sys.exit(1)


if __name__ == "__main__":
    main()
