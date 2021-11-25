(function (document, window, navigator) {
  // polyfill for when forEach is not supported e.g. IE11
  if ('NodeList' in window && !NodeList.prototype.forEach) {
    NodeList.prototype.forEach = function (callback, scope) {
      for (let i = 0; i < this.length; i++) {
        callback.call(scope || window, this[i], i, this)
      }
    }
  }

  // copy buttons
  (function (window, document, navigator) {
    const activeClassName = 'copied-to-clipboard'
    function copy (event) {
      event.preventDefault()
      const el = event.currentTarget
      if (navigator.clipboard) {
        navigator.clipboard.writeText(el.dataset.clip).then(function () {
          resetCopyButtons()
          el.classList.add(activeClassName)
        }, function (e) {
          console.error(e)
        })
      } else if (window.clipboardData) {
        window.clipboardData.setData('Text', el.dataset.clip)
        resetCopyButtons()
        el.classList.add(activeClassName)
      }
    }

    function resetCopyButtons () {
      document.querySelectorAll('button.' + activeClassName)
        .forEach(function (el) {
          el.classList.remove(activeClassName)
        })
    }
    document.querySelectorAll('button.copy-to-clipboard')
      .forEach(function (el) {
        el.addEventListener('click', copy)
      })
  })(window, document, navigator)
  // end copy buttons

  document.querySelectorAll('a[href="#print-dialogue"]')
    .forEach(function(link) {
      link.addEventListener('click', function(event) {
        event.preventDefault();
        window.print();
      })
    })
})(document, window, navigator);
