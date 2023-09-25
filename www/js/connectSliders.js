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
  // Function to apply custom styles to the target slider
  function addExtraDotLabel(slider_id){
    
    function startDragging(e, dot, minDot, maxDot, range=[0,1]) {
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
        let newX = ((e.clientX - dot.offsetWidth - sliderRect.x)/ sliderRect.width)*100;
        
        newX = Math.min(Math.max(newX, min), max);
        // Update the circle's position
        dot.style.left = newX + "%";
        label.textContent = Math.round((range[1] - range[0])*newX)/100+ "";
        label.style.left = newX + 0.25 +  "%";
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
    middleLabel.textContent = Math.round(avg_percent)/100;
    
    // insert rightDot, middleDot, middleLabel
    rightDot.parentNode.insertBefore(middleDot, rightDot.nextSibling);
    middleDot.parentNode.insertBefore(middleLabel, middleDot.nextSibling);
    
    middleDot.addEventListener("mousedown", function(e){startDragging(e, middleDot, rightDot, leftDot)});
  }
  
  function updateSliderBoundsForExtraDot(slider_id) {
    // Retrieve the max and min values from the control slider
    let slider = $("#"+ slider_id +"-label").next();
      
    let rightDot    = slider.find(".irs-handle.from")[0];
    let leftDot     = slider.find(".irs-handle.to")[0];
    let middleDot   = slider.find(".irs-handle.mid")[0];
    let middleLabel = slider.find(".irs-from.mid")[0];
    
    let middleL = percentageToNumber(middleDot.style.left);
    
    middleL = Math.max(percentageToNumber(rightDot.style.left), middleL);
    middleL = Math.min(percentageToNumber(leftDot.style.left), middleL);
    middleDot.style.left = middleL + "%";
    middleLabel.style.left = middleL + "%";
  }
  
  // Listen for changes in the control slider values
  Shiny.addCustomMessageHandler("updateSliderBoundsForExtraDot", updateSliderBoundsForExtraDot);
  Shiny.addCustomMessageHandler("addExtraDotLabel", addExtraDotLabel);
});