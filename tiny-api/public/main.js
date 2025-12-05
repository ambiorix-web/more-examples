const fileInput = document.getElementById("file");
const uploadBtn = document.getElementById("upload");
const MAX_SIZE = 5 * 1024 * 1024; // 5mb

fileInput.addEventListener("change", function (event) {
  const file = event.target.files[0];
  if (file && file.size > MAX_SIZE) {
    alert("File exceeds 5MB limit");
    fileInput.value = "";
    uploadBtn.classList.add("d-none");
    return;
  }

  uploadBtn.classList.remove("d-none");
});
