import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, Executable } from 'vscode-languageclient/node';
import * as path from 'path';

let client: LanguageClient | undefined;

function serverExecutablePath(ctx: vscode.ExtensionContext): string {
  const exe = process.platform === 'win32' ? 'mindscript-lsp.exe' : 'mindscript-lsp';
  // server/ sits at the extension root (same level as package.json)
  const uri = vscode.Uri.joinPath(ctx.extensionUri, 'server', exe);
  return uri.fsPath;
}

export async function activate(context: vscode.ExtensionContext) {
  const command = serverExecutablePath(context); // absolute path inside the extension
  const executable: Executable = {
    command,
    args: [],
    options: {
      cwd: vscode.Uri.joinPath(context.extensionUri).fsPath // extension root as CWD
    }
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'mindscript' }, { scheme: 'untitled', language: 'mindscript' }]
  };

  client = new LanguageClient('mindscript', 'MindScript Language Server', executable, clientOptions);
  client.outputChannel.show(true); // surface logs
  await client.start();
}

export async function deactivate() { await client?.stop(); }
