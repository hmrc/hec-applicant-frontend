(function (document, window, navigator) {

  // copy buttons
  (function (document, navigator) {
    const activeClassName = 'copied-to-clipboard'
    function copy(event) {
      const el = event.currentTarget
      return navigator.clipboard.writeText(el.dataset.clip).then(function () {
          resetCopyButtons()
          el.classList.add(activeClassName)
          el.blur()
        }, function (e) {
          console.error(e)
        }
      )
    }
    function resetCopyButtons() {
      document.querySelectorAll(`button.${activeClassName}`)
        .forEach(function (el) {
          el.classList.remove(activeClassName)
        })
    }
    document.querySelectorAll('button.copy-to-clipboard')
      .forEach(function (el) {
        el.addEventListener('click', copy)
      })
  })(document, navigator)
  // end copy buttons

  document.querySelectorAll('a[href="#print-dialogue"]')
    .forEach(function(link) {
      link.addEventListener('click', function(event) {
        event.preventDefault();
        window.print();
      })
    })
})(document, window, navigator);
