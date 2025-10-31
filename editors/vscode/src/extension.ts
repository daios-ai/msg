import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, Executable } from 'vscode-languageclient/node';
import * as fs from 'fs';

let client: LanguageClient | undefined;

function resolveServerCommand(ctx: vscode.ExtensionContext): string {
  // Allow user override via setting (relative to extension root)
  const configured = vscode.workspace.getConfiguration('mindscript').get<string>('serverPath');
  if (configured && configured.trim().length > 0) {
    const rel = vscode.Uri.joinPath(ctx.extensionUri, configured).fsPath;
    if (fs.existsSync(rel)) return rel;
  }

  // Default to bundled binary
  const bundled = vscode.Uri.joinPath(ctx.extensionUri, 'bin', 'msg-lsp').fsPath;
  if (fs.existsSync(bundled)) return bundled;

  // Fallback to PATH (useful in dev if installed globally)
  return 'msg-lsp';
}

export async function activate(context: vscode.ExtensionContext) {
  const command = resolveServerCommand(context);

  const executable: Executable = {
    command,
    args: ['--stdio'], // typical for Go LSP servers
    options: { cwd: context.extensionUri.fsPath }
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: 'file', language: 'mindscript' },
      { scheme: 'untitled', language: 'mindscript' }
    ]
  };

  client = new LanguageClient('MindScript', 'MindScript Language Server', executable, clientOptions);

  // Optional: expose a command to (re)start explicitly
  context.subscriptions.push(
    vscode.commands.registerCommand('mindscript.startServer', async () => {
      if (client?.isRunning()) {
        await client?.stop();
      }
      await client?.start();
      vscode.window.showInformationMessage('MindScript language server started.');
    })
  );

  // Start on activation (opening a .ms file will activate due to onLanguage)
  await client.start();
}

export async function deactivate() {
  await client?.stop();
}

