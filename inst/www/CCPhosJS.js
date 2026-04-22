

document.body.style.backgroundColor = "skyblue";


$(document).ready(function() { window.onload = function(){ $('.ui.accordion').accordion(); }; });


$('#table').on('shiny:value', function(event) {
    $("#table tr").click(function(){
         $(this).addClass('TableRowSelected').siblings().removeClass('TableRowSelected');
         var value=$(this).find('td:first').html();
         alert(value);
      });
})




shinyjs.SelectTableRow = function(){

  $(this).addClass("TableRowSelected").siblings().removeClass("TableRowSelected");
  var value=$(this).find('td:first').html();
  alert(value);

}

