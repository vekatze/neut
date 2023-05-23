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
        "alias",
        "attach",
        "by",
        "case",
        "default",
        "define",
        "define-inline",
        "detach",
        "else",
        "else-if",
        "export",
        "if",
        "import",
        "in",
        "introspect",
        "lambda",
        "let",
        "let?",
        "match",
        "mu",
        "on",
        "resource",
        "struct",
        "switch",
        "variant",
        "with",
      ].join(" "),
    },
    contains: [
      { className: "type",
        begin: "tau|&|:<|flow|[A-Z][a-z0-9A-Z]*",
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
        begin: "<=|->|=>|*|:|this|&|-(?=\\s)|tuple|magic",
      },
      hljs.COMMENT(
        "//", // begin
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
