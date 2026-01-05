/* prism-mindscript.js */
(function (Prism) {
  Prism.languages.mindscript = {
    // Annotations/comments: '#' to end-of-line (newline not included in match)
    comment: {
      pattern: /#[^\n]*/,
      greedy: true
    },

    string: {
      pattern: /(["'])(?:\\(?:x[\da-fA-F]{2}|u[\da-fA-F]{4}|[\s\S])|(?!\1)[^\\])*\1/,
      greedy: true
    },

    function: {
      pattern: /(\b(?:fun|oracle)\s+)[A-Za-z_][A-Za-z0-9_]*/,
      lookbehind: true
    },

    'class-name': /\b(?:Type|Null|Str|Int|Num|Bool|Any|Handle|Enum)\b/,

    keyword: /\b(?:and|or|not|let|do|end|return|break|continue|if|then|elif|else|fun|oracle|module|for|in|from|while|type)\b/,

    boolean: /\b(?:true|false)\b/,
    null: /\bnull\b/,

    number: [
      /\b0(?:b|B)[01](?:_?[01])*\b/,
      /\b0(?:o|O)[0-7](?:_?[0-7])*\b/,
      /\b0(?:x|X)[\da-fA-F](?:_?[\da-fA-F])*\b/,
      /\b\d(?:_?\d)*(?:\.(?:\d(?:_?\d)*)?)?(?:[eE][+\-]?\d(?:_?\d)*)?\b/,
      /(?:^|[^\w.])\.(?:\d(?:_?\d)*)?(?:[eE][+\-]?\d(?:_?\d)*)?\b/
    ],

    operator: /\*\*|->|==|!=|<=|>=|<<|>>|[+\-*/%=&|^~!<>]/,

    punctuation: /[()[\]{}:,.;.?]/,

    identifier: /\b[A-Za-z_][A-Za-z0-9_]*\b/
  };

  Prism.languages.ms = Prism.languages.mindscript;
})(Prism);

