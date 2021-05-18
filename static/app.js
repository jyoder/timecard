function scrollToPinned() {
    let anchors = $('.scroll-pinned');
    for (let i = 0; i < anchors.length; i++) {
        anchors[i].scrollIntoView();
    }
}

$(document).on('ready turbolinks:load', function () {
    // Initialize htmx whenever Turbolinks loads a new page
    htmx.process(document.body);
    // For anything marked as 'scroll-pinned', scroll to it on page load
    scrollToPinned();
});

$(document).on("htmx:afterOnLoad", function () {
    // Reformat time elements based on locale whenever page changes occur
    initTime();
    // Scroll to any elements marked as 'scroll-pinned' whenever page changes occur
    scrollToPinned();
});