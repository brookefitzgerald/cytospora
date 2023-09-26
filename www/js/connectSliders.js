$(document).ready(function() {
  function percentageToNumber(percentageString) {
      // Remove the "%" character from the string
      var cleanedString = percentageString.replace('%', '');
    
      // Parse the cleaned string as a float
      var number = parseFloat(cleanedString);
    
      // Check if parsing was successful
      if (!isNaN(number)) {
        // If parsing was successful, return the number
        return number;
      } else {
        // If parsing failed, return an error value or handle it as needed
        return null; // You can choose a different error handling strategy
      }
    }
  function createMiddleDot(slider_data){
    const slider_id = slider_data[0];
    const range = [parseFloat(slider_data[1]), parseFloat(slider_data[2])];
    const start = slider_data[3];
    function startDragging(e, dot, minDot, maxDot) {
      isDragging = true;
      const label = dot.nextSibling; 
      $(document).on("mousemove", onMouseMove);
      $(document).on("mouseup", stopDragging);
      
      function onMouseMove(e) {
        if (!isDragging) return;
        const sliderRect = dot.parentElement.getBoundingClientRect();
        // Calculate the new position of the circle
        let min = percentageToNumber(minDot.style.left);
        let max = percentageToNumber(maxDot.style.left);
        let newX = ((e.clientX - dot.offsetWidth - sliderRect.x)/ (sliderRect.width))*100;
        newX = Math.min(Math.max(newX, min), max);
        
        // Update the circle's position
        dot.style.left = newX + "%";
        label.style.left = newX + 0.25 +  "%";
        
        // Update the circle's label (shoudl not include the width of the dot)
        let newLabelX = newX *(sliderRect.width) / (sliderRect.width - dot.offsetWidth);
        newLabelX = Math.round((range[1] - range[0])*newLabelX)/100+ range[0];
        label.textContent = newLabelX + "";
        
        Shiny.setInputValue(slider_id + "_avg", newLabelX);
      }
  
      function stopDragging() {
        isDragging = false;
        $(document).off("mousemove", onMouseMove);
        $(document).off("mouseup", stopDragging);
      }
    }
    
    let slider = $("#"+ slider_id +"-label").next();
    
    let rightDot = slider.find(".irs-handle.from")[0];
    let leftDot = slider.find(".irs-handle.to")[0];
    
    let middleDot = rightDot.cloneNode(true);
    middleDot.classList.add('mid');
    middleDot.style.background = "#428BCA";
    
    let avg_percent = (percentageToNumber(leftDot.style.left) + percentageToNumber(rightDot.style.left))/2;
    middleDot.style.left = avg_percent + "%";
    
    let rightLabel =  slider.find(".irs-from")[0];
    middleLabel = rightLabel.cloneNode(true);
    middleLabel.classList.add('mid');
    
    middleLabel.style.left = avg_percent + "%";
    middleLabel.textContent = start;
    Shiny.setInputValue(slider_id + "_avg", start);
    // insert in this order: rightDot, middleDot, middleLabel
    rightDot.parentNode.insertBefore(middleDot, rightDot.nextSibling);
    middleDot.parentNode.insertBefore(middleLabel, middleDot.nextSibling);
    
    middleDot.addEventListener("mousedown", function(e){startDragging(e, middleDot, rightDot, leftDot)});
  }
  // Function to apply custom styles to the target slider
  function addExtraDotLabel(slider_data){
    createMiddleDot(slider_data);
  }
  
  function updateSliderBoundsForExtraDot(slider_data) {
    const slider_id = slider_data[0];
    const range = [parseFloat(slider_data[1]), parseFloat(slider_data[2])];

    // Retrieve the max and min values from the control slider
    let slider = $("#"+ slider_id +"-label").next();
      
    let rightDot    = slider.find(".irs-handle.from")[0];
    let leftDot     = slider.find(".irs-handle.to")[0];
    let middleDot   = slider.find(".irs-handle.mid")[0];
    let middleLabel = slider.find(".irs-from.mid")[0];
    
    let sliderRect = rightDot.parentElement.getBoundingClientRect();

    if (typeof middleDot !== 'undefined') {
      let middleL = percentageToNumber(middleDot.style.left);
      middleL = Math.max(percentageToNumber(rightDot.style.left), middleL);
      middleL = Math.min(percentageToNumber(leftDot.style.left), middleL);
      
      middleDot.style.left = middleL + "%";
      middleLabel.style.left = middleL + "%";
      
      let newLabelX = middleL*(sliderRect.width) / (sliderRect.width - rightDot.offsetWidth);
      newLabelX = Math.round((range[1] - range[0])*newLabelX)/100+ range[0];
      middleLabel.textContent = newLabelX + "";
      Shiny.setInputValue(slider_id + "_avg", newLabelX);
    }
    
  }
  
  // Listen for changes in the control slider values
  Shiny.addCustomMessageHandler("updateSliderBoundsForExtraDot", updateSliderBoundsForExtraDot);
  Shiny.addCustomMessageHandler("addExtraDotLabel", addExtraDotLabel);
});