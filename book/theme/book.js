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
        "case",
        "codata",
        "data",
        "default",
        "define",
        "else",
        "else-if",
        "end",
        "enum",
        "fix",
        "if",
        "import",
        "in",
        "introspect",
        "lambda",
        "let",
        "let?",
        "match",
        "match-noetic",
        "new",
        "on",
        "record",
        "resource",
        "section",
        "struct",
        "switch",
        "then",
        "use",
        "variant",
        "with",
      ].join(" "),
    },
    contains: [
      { className: "type",
        begin: "tau|&",
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
        begin: "<=|->|=>|:|&|-(?=\\s)",
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
    tabReplace: "  ", // 4 spaces
    languages: [], // Languages used for auto-detection
  });

  let code_nodes = Array.from(document.querySelectorAll("code"))
    // Don't highlight `inline code` blocks in headers.
    .filter(function (node) {
      return !node.parentElement.classList.contains("header");
    });

  code_nodes.forEach(function (block) {
    hljs.highlightBlock(block);
  });

  Array.from(document.querySelectorAll("pre code")).forEach(function (block) {
    var pre_block = block.parentNode;
    var buttons = pre_block.querySelector(".buttons");
    if (!buttons) {
      buttons = document.createElement("div");
      buttons.className = "buttons";
      pre_block.insertBefore(buttons, pre_block.firstChild);
    }

    var clipButton = document.createElement("button");
    clipButton.className = "clip-button";
    clipButton.title = "Copy to clipboard";
    clipButton.setAttribute("aria-label", clipButton.title);
    clipButton.innerHTML = "<span>Copy</span>";
    buttons.insertBefore(clipButton, buttons.firstChild);
    // }
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

(function clipboard() {
  var clipButtons = document.querySelectorAll(".clip-button");

  function hideTooltip(elem) {
    elem.firstChild.innerText = "Copy";
    elem.className = "clip-button";
  }

  function showTooltip(elem, msg) {
    elem.firstChild.innerText = msg;
    elem.className = "tooltipped";
  }

  var clipboardSnippets = new ClipboardJS(".clip-button", {
    text: function (trigger) {
      hideTooltip(trigger);
      let playground = trigger.closest("pre");
      return playground_text(playground, false);
    },
  });

  Array.from(clipButtons).forEach(function (clipButton) {
    clipButton.addEventListener("mouseout", function (e) {
      hideTooltip(e.currentTarget);
    });
  });

  clipboardSnippets.on("success", function (e) {
    e.clearSelection();
    showTooltip(e.trigger, "✓ Copied!");
  });

  clipboardSnippets.on("error", function (e) {
    showTooltip(e.trigger, "× Error");
  });
})();

if (window.matchMedia('(max-width: 900px)').matches) {
  document.querySelector("#body-container").focus();
} else {
  document.querySelector("#content").focus();
}
