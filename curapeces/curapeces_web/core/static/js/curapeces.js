function PreviewImage() {
    var oFReader = new FileReader();
    oFReader.readAsDataURL(document.getElementById("inputimge").files[0]);

    oFReader.onload = function (oFREvent) {
        document.getElementById("image").src = oFREvent.target.result;
        
    };
};