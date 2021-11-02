function scrollToPinned() {
    let elements = $('.scroll-to-pinned');
    for (let i = 0; i < elements.length; i++) {
        elements[i].scrollIntoView();
    }
}

function initTimePicker() {
    flatpickr(".flatpickr-time-input", {
        allowInput: true,
        enableTime: true,
        noCalendar: true,
        dateFormat: "H:i:S",
        altInput: true,
        altFormat: "h:i K",
        disableMobile: true,
        onClose(dates, currentdatestring, picker) {
            picker.setDate(picker.altInput.value, true, picker.config.altFormat)
        }
    });
}

function initTooltip() {
    $('[data-toggle="tooltip"]')
        .tooltip('dispose')
        .click(function () {
            let self = $(this);
            self.tooltip('show');
            self.mouseout(function () {
                self.tooltip('dispose');
            });
        });
}

function initApp() {
    // For anything with class 'scroll-to-pinned', scroll to that element
    scrollToPinned();

    // Reformat time elements based on locale whenever page changes occur
    initTime();
    initTimePicker();

    // Initialize tooltips on tooltip elements
    initTooltip();
}

$(document).on('ready turbolinks:load', function () {
    initApp();
});
