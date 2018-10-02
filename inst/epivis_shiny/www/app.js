$(function() {
   $('body').on('click', '#removeActionButton', function() {
    console.log("Yes, I have been clicked");
    Shiny.setInputValue('divItem',document.getElementById("#removeActionButton").parentElement.nodeName);
  });
})