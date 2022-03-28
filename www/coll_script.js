function uncheck(form) {
    var c = document.getElementById(form).getElementsByTagName('input');
    for (var i = 0; i < c.length; i++) {
        if (c[i].type == 'checkbox') {
            c[i].checked = false;
        }
    }
}