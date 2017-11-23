app.ports.storeSession.subscribe(function (session) {
    localStorage.session = session;
});


window.addEventListener("storage", function (event) {
    if (event.storageArea === localStorage && event.key === "session") {
        app.ports.onSessionChange.send(event.newValue);
    }
}, false);


app.ports.importQuestion.subscribe(function (question) {
    var component = document.querySelector('myscript-math-web');
    var importContent = document.getElementById('importContentField');

    function actualImport(component, question) {
        component.importContent({
            x: 0,
            y: 0
        }, question, "application/x-latex");
        //component.convert();
    }

    if (component.hasAttribute("initialized")) {
        actualImport(component, question);
    } else {
        component.addEventListener("initialized-changed", function (event) {
            actualImport(component, question);
        });
    }

});