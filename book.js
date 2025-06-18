"use strict";

// Fix back button cache problem
window.onunload = function () {};

function playground_text(playground, hidden = true) {
  let code_block = playground.querySelector("code");
  if (hidden) {
    return code_block.textContent;
  } else {
    return code_block.innerText;
  }
}

hljs.registerLanguage("neut", function (hljs) {
  return {
    keywords: {
      $pattern: /[\w-]+/,
      keyword: [
        "attach",
        "box",
        "case",
        "data",
        "default",
        "define",
        "detach",
        "else",
        "else-if",
        "exact",
        "foreign",
        "function",
        "if",
        "import",
        "in",
        "inline",
        "introspect",
        "let",
        "letbox",
        "letbox-T",
        "match",
        "nominal",
        "of",
        "on",
        "pin",
        "quote",
        "resource",
        "tie",
        "try",
        "use",
        "when",
      ].join(" "),
      builtin: [
        "_",
        "assert",
        "include-text",
        "magic",
        "static",
        "this",
        "new-cell",
        "new-channel",
      ],
      type: [
        "meta",
        "pointer",
        "rune",
        "thread",
        "type",
        "void",
      ],
    },
    contains: [
      {
        begin: [
          /\s/,
          /[A-Z][a-z0-9A-Z-]*/,
          /[^\w-=<>]/
        ],
        beginScope: {
          2: "constructor"
        },
      },
      {
        begin: [
          /(define|inline|data|resource)/,
          /\s+/,
          /[\w-]+/,
        ],
        beginScope: {
          3: "definition"
        },
      },
      {
        scope: "string",
        begin: '"',
        end: '"',
        contains: [hljs.BACKSLASH_ESCAPE],
      },
      {
        scope: "string",
        begin: '`',
        end: '`',
        contains: [hljs.BACKSLASH_ESCAPE],
      },
      { scope: "warning",
        begin: "admit",
      },
      {
        scope: "builtin",
        begin: "\\*|\\||->|=>|:|!|,|;|&|=",
      },
      hljs.COMMENT("//", "$"),
    ],
  };
});

hljs.registerLanguage("ens", function (hljs) {
  return {
    keywords: {
      $pattern: /[\w-]+/,
    },
    contains: [
      { scope: "type",
        begin: "true|false",
      },
      {
        begin: [
          /^\s+/,
          /[\w-]+/,
          /\s+/,
        ],
        beginScope: {
          2: "keyword"
        },
      },
      {
        scope: "string",
        begin: '"',
        end: '"',
        contains: [hljs.BACKSLASH_ESCAPE],
      },
      hljs.COMMENT("//", "$"),
    ],
  };
});

hljs.registerLanguage("llvm", function (hljs) {
  return {
    keywords: {
      $pattern: /[\w-:=<>]+/,
      keyword: [
        "define",
        "declare",
        "private",
        "constant",
      ].join(" "),
    },
    contains: [
      { scope: "type",
        begin: "\\bptr\\b|i64|i8|float|double|x86_fp80|fp128|ppc_fp128",
      },
      {
        scope: "string",
        begin: '"',
        end: '"',
        contains: [hljs.BACKSLASH_ESCAPE],
      },
      {
        scope: "builtin",
        begin: "unnamed_addr|add|ptrtoint|inttoptr|getelementptr|call|store|load|null|fastcc",
      },
      hljs.COMMENT(";", "$"),
    ],
  };
});

(function codeSnippets() {
  // Syntax highlighting Configuration
  hljs.configure({
    tabReplace: "  ",
    languages: [],
  });

  let code_nodes = Array.from(document.querySelectorAll("code"))
  // Don't highlight `inline code` blocks in headers.
      .filter(function (node) {
        return !node.parentElement.classList.contains("header");
      });

  code_nodes.forEach(function (block) {
    hljs.highlightElement(block);
  });
})();

(function chapterNavigation() {
  document.addEventListener("keydown", function (e) {
    if (e.altKey || e.ctrlKey || e.metaKey || e.shiftKey) {
      return;
    }
    if (window.search && window.search.hasFocus()) {
      return;
    }

    switch (e.key) {
    case "ArrowRight":
      e.preventDefault();
      var nextButton = document.querySelector(".nav.next");
      if (nextButton) {
        window.location.href = nextButton.href;
      }
      break;
    case "ArrowLeft":
      e.preventDefault();
      var previousButton = document.querySelector(".nav.previous");
      if (previousButton) {
        window.location.href = previousButton.href;
      }
      break;
    }
  });
})();

if (window.matchMedia('(max-width: 900px)').matches) {
  document.querySelector("#body-container").focus();
} else {
  document.querySelector("#content").focus();
}
