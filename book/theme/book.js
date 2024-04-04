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
      $pattern: /[\w-:=<>]+/,
      keyword: [
        "attach",
        "bind",
        "case",
        "constant",
        "data",
        "declare",
        "default",
        "define",
        "detach",
        "else",
        "else-if",
        "exact",
        "export",
        "foreign",
        "function",
        "if",
        "import",
        "in",
        "inline",
        "introspect",
        "let",
        "match",
        "nominal",
        "of",
        "on",
        "resource",
        "switch",
        "tie",
        "try",
        "use",
        "when",
        "with",
      ].join(" "),
    },
    contains: [
      { className: "type",
        begin: "tau|&|:<|flow|\\b[A-Z][a-z0-9A-Z-]*",
      },
      {
        className: "string",
        scope: "string",
        begin: '"',
        end: '"',
        contains: [hljs.BACKSLASH_ESCAPE],
      },
      { className: "warning",
        begin: "admit",
      },
      {
        className: "builtin",
        begin: "<=|->|=>|\\*|:|\\||this|,|;|_|&|\\b-(?=\\s)|=|magic|assert",
      },
      hljs.COMMENT(
        "//", // begin
        "$" // end
      ),
    ],
  };
});

hljs.registerLanguage("ens", function (hljs) {
  return {
    keywords: {
      $pattern: /[\w-:=<>]+/,
      keyword: [
        "antecedent",
        "archive",
        "build",
        "dependency",
        "foreign",
        "inline-limit",
        "prefix",
        "preset",
        "source",
        "target",
      ].join(" "),
    },
    contains: [
      { className: "type",
        begin: "tau|&|:<|flow|[A-Z][a-z0-9A-Z-]*",
      },
      {
        className: "string",
        scope: "string",
        begin: '"',
        end: '"',
        contains: [hljs.BACKSLASH_ESCAPE],
      },
      {
        className: "builtin",
        begin: "<=|->|=>|\\*|:|this|&|-(?=\\s)|tuple|magic",
      },
      hljs.COMMENT(
        "//", // begin
        "$" // end
      ),
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
      { className: "type",
        begin: "\\bptr\\b|i64|i8|float|double|x86_fp80|fp128|ppc_fp128",
      },
      {
        className: "string",
        scope: "string",
        begin: '"',
        end: '"',
        contains: [hljs.BACKSLASH_ESCAPE],
      },
      {
        className: "hole-suffix",
        begin: /\/[0-9]+/,
      },
      {
        className: "builtin",
        begin: "unnamed_addr|add|ptrtoint|inttoptr|getelementptr|call|store|load|null|fastcc",
      },
      hljs.COMMENT(
        ";", // begin
        "$" // end
      ),
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
    hljs.highlightBlock(block);
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
