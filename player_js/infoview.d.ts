// Type stub for @leanprover/infoview used by widget JS.
declare module "@leanprover/infoview" {
    interface RpcSession {
        call(method: string, params: unknown): Promise<any>;
    }
    export function useRpcSession(): RpcSession;

    interface Position {
        line: number;
        character: number;
    }
    interface Range {
        start: Position;
        end: Position;
    }
    interface TextEdit {
        range: Range;
        newText: string;
    }
    interface TextDocumentEdit {
        textDocument: { uri: string; version: number | null };
        edits: TextEdit[];
    }
    interface WorkspaceEdit {
        documentChanges?: TextDocumentEdit[];
    }
    interface EditorApi {
        applyEdit(edit: WorkspaceEdit): Promise<void>;
    }
    export interface EditorConnection {
        api: EditorApi;
    }
    export const EditorContext: import("react").Context<EditorConnection>;
}
