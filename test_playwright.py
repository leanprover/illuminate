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
Playwright visual and structural tests for Illuminate SVG output.

Run with:
    uv run test_playwright.py

Or for pytest mode:
    uv run pytest test_playwright.py -v

To update expected baselines:
    UPDATE_BASELINES=1 uv run test_playwright.py
"""
import os
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
# Visual test helpers
# ---------------------------------------------------------------------------

FONTS_DIR = VISUAL_DIR / "fonts"

def _screenshot_svg(page, svg_name: str) -> bytes:
    """Load an SVG in an HTML wrapper with bundled fonts and return its screenshot bytes."""
    svg_path = ROOT / svg_name
    assert svg_path.exists(), f"{svg_name} not found — run lake test first"
    svg_content = svg_path.read_text()
    # Rewrite generic font families to bundled DejaVu fonts
    svg_content = svg_content.replace('font-family="sans-serif"', 'font-family="DejaVu Sans"')
    svg_content = svg_content.replace('font-family="monospace"', 'font-family="DejaVu Sans Mono"')
    sans_font = (FONTS_DIR / "DejaVuSans.ttf").resolve().as_uri()
    mono_font = (FONTS_DIR / "DejaVuSansMono.ttf").resolve().as_uri()
    html = f"""<!DOCTYPE html>
<html><head><style>
@font-face {{ font-family: 'DejaVu Sans'; src: url('{sans_font}'); }}
@font-face {{ font-family: 'DejaVu Sans Mono'; src: url('{mono_font}'); }}
</style></head><body style="margin:0;padding:0">{svg_content}</body></html>"""
    page.set_content(html)
    page.wait_for_load_state("networkidle")
    screenshot = page.screenshot(full_page=True)
    assert len(screenshot) > 1000, f"Screenshot too small: {len(screenshot)} bytes"
    return screenshot


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


def _run_visual_test(page, svg_name: str, test_name: str):
    """Screenshot an SVG, write actual PNG, compare against expected, optionally update expected."""
    VISUAL_DIR.mkdir(exist_ok=True)
    screenshot = _screenshot_svg(page, svg_name)

    actual_path = VISUAL_DIR / f"{test_name}.actual.png"
    expected_path = VISUAL_DIR / f"{test_name}.expected.png"

    actual_path.write_bytes(screenshot)

    if UPDATE_BASELINES:
        expected_path.write_bytes(screenshot)

    if not expected_path.exists():
        raise AssertionError(
            f"No expected baseline at {expected_path.relative_to(ROOT)}. "
            f"Run with UPDATE_BASELINES=1 to create it."
        )

    baseline = expected_path.read_bytes()
    ratio = pixel_diff_ratio(screenshot, baseline)
    assert ratio < 0.001, (
        f"{test_name} visual regression: {ratio:.4%} pixels differ. "
        f"Compare {actual_path.name} vs {expected_path.name} in visual_tests/"
    )


# ---------------------------------------------------------------------------
# Structural tests
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


# ---------------------------------------------------------------------------
# Visual regression tests
# ---------------------------------------------------------------------------

def test_smiley_visual(page):
    """Compare smiley screenshot against expected baseline."""
    _run_visual_test(page, "smiley.svg", "smiley")


def test_commdiag_visual(page):
    """Compare commdiag screenshot against expected baseline."""
    _run_visual_test(page, "commdiag.svg", "commdiag")


def test_roundedrects_visual(page):
    """Compare rounded-rects screenshot against expected baseline."""
    _run_visual_test(page, "roundedrects.svg", "roundedrects")


def test_pipeline_visual(page):
    """Compare pipeline diagram screenshot against expected baseline."""
    _run_visual_test(page, "pipeline.svg", "pipeline")


def test_stringlayout_visual(page):
    """Compare string layout diagram screenshot against expected baseline."""
    _run_visual_test(page, "string-layout.svg", "stringlayout")


def test_coechain_visual(page):
    """Compare coe-chain diagram screenshot against expected baseline."""
    _run_visual_test(page, "coe-chain.svg", "coechain")


def test_lakeworkspace_visual(page):
    """Compare lake workspace diagram screenshot against expected baseline."""
    _run_visual_test(page, "lake-workspace.svg", "lakeworkspace")


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


def test_stars_visual(page):
    """Compare stars screenshot against expected baseline."""
    _run_visual_test(page, "stars.svg", "stars")


def test_star_anchors_visual(page):
    """Compare star-anchors screenshot against expected baseline."""
    _run_visual_test(page, "star-anchors.svg", "star-anchors")


def test_ellipse_visual(page):
    """Compare ellipse screenshot against expected baseline."""
    _run_visual_test(page, "ellipse.svg", "ellipse")


def test_transforms_visual(page):
    """Compare transforms screenshot against expected baseline."""
    _run_visual_test(page, "transforms.svg", "transforms")


def test_ghost_refocus_visual(page):
    """Compare ghost-refocus screenshot against expected baseline."""
    _run_visual_test(page, "ghost-refocus.svg", "ghost-refocus")


def test_cellophane_clip_structure(page):
    """Cellophane/clip SVG has opacity groups and clipPath elements."""
    svg_path = ROOT / "cellophane-clip.svg"
    assert svg_path.exists(), "cellophane-clip.svg not found — run lake test first"
    svg_content = svg_path.read_text()
    assert "opacity" in svg_content, "Missing opacity attribute for cellophane"
    assert "clipPath" in svg_content, "Missing clipPath element for clip"


def test_cellophane_clip_visual(page):
    """Compare cellophane-clip screenshot against expected baseline."""
    _run_visual_test(page, "cellophane-clip.svg", "cellophane-clip")


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

    tests = [
        test_smiley_structure,
        test_smiley_bounding_box,
        test_commdiag_structure,
        test_commdiag_labels,
        test_commdiag_annotations,
        test_commdiag_node_positions,
        test_smiley_visual,
        test_commdiag_visual,
        test_roundedrects_visual,
        test_pipeline_visual,
        test_stringlayout_visual,
        test_coechain_visual,
        test_lakeworkspace_visual,
        test_stars_structure,
        test_stars_visual,
        test_star_anchors_visual,
        test_ellipse_visual,
        test_transforms_visual,
        test_ghost_refocus_visual,
        test_cellophane_clip_structure,
        test_cellophane_clip_visual,
    ]

    passed = 0
    failed = 0

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)

        for test_fn in tests:
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

    print(f"\n{passed} passed, {failed} failed")
    if failed > 0:
        sys.exit(1)


if __name__ == "__main__":
    main()
