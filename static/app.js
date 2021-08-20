function scrollToEnd() {
    let elements = $('.scroll-to-end');
    for (let i = 0; i < elements.length; i++) {
        elements[i].scrollTop = 1000000000;
    }
}

function initDatePicker() {
    if (!('flatpickr' in window)) {
        return;
    }
    flatpickr("input[type='date']", {
        allowInput: true,
        dateFormat: "Y-m-d",
        altInput: true,
        altFormat: "m/d/Y"
    });
    flatpickr("input[type='datetime-local']", {
        allowInput: true,
        enableTime: false,
        dateFormat: 'Z',
        altInput: true,
        altFormat: 'm/d/Y',
    });
}

function initTimePicker() {
    flatpickr(".flatpickr-time-input", {
        allowInput: true,
        enableTime: true,
        noCalendar: true,
        dateFormat: "H:i:S",
        altInput: true,
        altFormat: "h:i K",
        onClose(dates, currentdatestring, picker) {
            picker.setDate(picker.altInput.value, true, picker.config.altFormat)
        }
    });
}

function initApp() {
    // For anything with class 'scroll-to-end', scroll that element to the end on page load
    scrollToEnd();

    // Reformat time elements based on locale whenever page changes occur
    initTime();
    initTimePicker();
}

$(document).on('ready turbolinks:load', function () {
    initApp();
});
