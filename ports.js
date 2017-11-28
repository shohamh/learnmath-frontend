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

app.ports.myscriptConvert.subscribe(function () {
    var commonElements = Array.from(document.getElementsByTagName('myscript-common-element'));
    console.log("myscriptConvert (auto-resize)");
    commonElements.forEach(function (commonElement) {
        if (!(commonElement.hasAttribute("canexport") && commonElement.hasAttribute("canconvert")))
        {
            var fakeStroke = {
                "-myscript-pen-fill-color": "#FFFFFF00",
                "-myscript-pen-fill-style": "none",
                "-myscript-pen-width": 1,
                "color": "#1580CD",
                "id": "pendingStroke-0",
                "l": [ ],
                "p": [ ],
                "pointerId": -1,
                "pointerType": "pen",
                "t": [ ],
                "type": "stroke",
                "width": 0,
                "x": [],
                "y": [ ]
            };
            commonElement.canexport = "";
            commonElement.canconvert = "function L(t,n,e){var r=et.setRecognitionContext(t,{model:n,callback:function(t,r){return ut(n,t,r,e)}});ot.sendMessage(r,b).catch(function(r){return ot.retry(L,t,n,e)})}";
            commonElement.editor.model.rawStrokes = [fakeStroke];
        }
        commonElement.convert();
    });
});