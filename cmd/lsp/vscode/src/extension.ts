import * as path from 'path';
import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext) {
  const cfg = vscode.workspace.getConfiguration('mindscript');
  const serverPath = cfg.get<string>('serverPath') || '';

  if (!serverPath) {
    vscode.window.showWarningMessage(
      'MindScript: set "mindscript.serverPath" to your LSP binary to enable language features.'
    );
    return;
  }

  const serverOptions: ServerOptions = {
    run:   { command: serverPath, transport: TransportKind.stdio },
    debug: { command: serverPath, transport: TransportKind.stdio }
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: 'mindscript', scheme: 'file' }, { language: 'mindscript', scheme: 'untitled' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.ms')
    }
  };

  client = new LanguageClient(
    'mindscriptLanguageServer',
    'MindScript Language Server',
    serverOptions,
    clientOptions
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('mindscript.startServer', async () => {
      if (client) {
        // `start()` may return a Thenable in newer clients; `await` is fine.
        await client.start();
      } else {
        vscode.window.showErrorMessage('MindScript LSP client is not initialized.');
      }
    })
  );

  await client.start();
}

export function deactivate(): Promise<void> | undefined {
  return client?.stop();
}


