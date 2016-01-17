Shiny.addCustomMessageHandler('predicting', function(message){
    if (message.busy === true) {
        $('html').addClass('predicting-busy');
        setTimeout(function() {
            if ($('html').hasClass('predicting-busy')) {
                $('#predictingBusy').tooltip('show');
            }
        }, 500);
    } else {
        $('html').removeClass('predicting-busy');
        $('#predictingBusy').tooltip('hide');
    }
});