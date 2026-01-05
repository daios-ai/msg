/* prism-mindscript-repl.js
 *
 * Prism language definition for MindScript REPL transcripts.
 *
 * Token colors intended:
 * - prompt lines (==> / ...) : normal (inherit)
 * - annotations (# ... end-of-line): green (use Prism's .token.comment)
 * - everything else: blue (use .token.output)
 */
(function (Prism) {
  Prism.languages['mindscript-repl'] = {
    // Prompt lines: start with "==>" or "..."
    prompt: {
      pattern: /(^|\n)(?:==>|\.{3})[^\n]*/g,
      lookbehind: true,
      greedy: true,
      inside: {
        comment: /#.*/
      }
    },

    // Non-prompt lines: everything else (outputs, blank-ish lines with content)
    output: {
      pattern: /(^|\n)(?!\s*(?:==>|\.{3}))[^\n]+/g,
      lookbehind: true,
      greedy: true,
      inside: {
        comment: /#.*/
      }
    }
  };

  // Optional short alias
  Prism.languages.msrepl = Prism.languages['mindscript-repl'];
})(Prism);

