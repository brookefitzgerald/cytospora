$(document).ready(function() {
  var plot = document.getElementById('plot')

  plot.addEventListener('touchmove', function (e) {
    var touch = e.changedTouches[0];
    var mouseEvent = new MouseEvent('mousemove', {
      view: window,
      bubbles: true,
      cancelable: true,
      screenX: touch.screenX,
      screenY: touch.screenY,
      clientX: touch.clientX,
      clientY: touch.clientY
    })
    touch.target.dispatchEvent(mouseEvent);
    e.preventDefault()
  }, { passive: false });

  plot.addEventListener('touchstart', function(e) {
    Shiny.onInputChange('draw', true)
    e.preventDefault()
  }, { passive: false });

  plot.addEventListener('touchend', function(e) {
    Shiny.onInputChange('draw', false)
    e.preventDefault()
  }, { passive: false });
});
