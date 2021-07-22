function scrollToPinned() {
    let anchors = $('.scroll-pinned');
    for (let i = 0; i < anchors.length; i++) {
        anchors[i].scrollIntoView();
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
        altFormat: "h:i K"
    });
}

function initCustomTime() {
    document.querySelectorAll('.weekday').forEach(function (elem) {
        var date = new Date(elem.dateTime);
        elem.innerHTML = date.toLocaleDateString([], { weekday: 'long' });
    });
}

function initApp() {
    // For anything marked as 'scroll-pinned', scroll to it on page load
    scrollToPinned();

    // Reformat time elements based on locale whenever page changes occur
    initTime();
    initCustomTime();
    initTimePicker();
}

$(document).on('ready turbolinks:load', function () {
    initApp();
});
