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
        altFormat: 'd.m.y',
    });
    flatpickr("input[type='datetime-local']", {
        allowInput: true,
        enableTime: false,
        dateFormat: 'Z',
        altInput: true,
        altFormat: 'm/d/Y',
    });
}

function initCustomTime() {
    document.querySelectorAll('.weekday').forEach(function (elem) {
        var date = new Date(elem.dateTime);
        elem.innerHTML = date.toLocaleDateString([], { weekday: 'long' });
    });
}

function initApp() {
    // Initialize htmx whenever Turbolinks loads a new page
    htmx.process(document.body);
    // Reformat time elements based on locale whenever page changes occur
    initTime();
    initCustomTime();
    // For anything marked as 'scroll-pinned', scroll to it on page load
    scrollToPinned();
}

$(document).on('ready turbolinks:load', function () {
    initApp();
});

$(document).on("htmx:afterOnLoad", function () {
    initApp();
});
