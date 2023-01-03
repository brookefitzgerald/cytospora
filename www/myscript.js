Shiny.addCustomMessageHandler('updateSliders', function(e) {
  console.log(e);
  console.log('testing!');
  $('.irs-min').each(function() {
    this.textContent = this.textContent.replace(',', '');
  });
  
  $('.irs-max').each(function() {
    this.textContent = this.textContent.replace(',', '');
  });
  
  $('.irs-single').each(function() {
    this.textContent = this.textContent.replace(',', '');
  });
  
  $('.irs-grid-text').each(function() {
    this.textContent = this.textContent.replace(',', '');
  });
});