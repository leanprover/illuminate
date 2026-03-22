// Type stub for @leanprover/infoview used by widget JS.
declare module "@leanprover/infoview" {
    interface RpcSession {
        call(method: string, params: unknown): Promise<any>;
    }
    export function useRpcSession(): RpcSession;
}
