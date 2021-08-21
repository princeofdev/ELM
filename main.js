const app = Elm.Main.init({
  flags: { width: window.innerWidth, height: window.innerHeight, year : ( new Date().getFullYear()), storage: JSON.parse(localStorage.getItem("storage")) },
});

app.ports.controlVideo.subscribe((message) => {
  var vid = document.getElementById("earthVideo");
  window.requestAnimationFrame(function() {
    if (message) {
      if (vid) {
        vid.muted = true;
        vid.play();
      }
    } else vid.pause();
  });
});

app.ports.setCursor.subscribe((pos) => {
  try {
    var input = document.activeElement;
    console.log(pos)
    input.focus();
    input.setSelectionRange(pos, pos);
  } catch {}
});

app.ports.waitForId.subscribe((id) => {
  //app.ports.idLoaded.send(id);
  item = document.getElementById(id)
  if (item && item.firstChild) {
    item.firstChild.onload = function(){ app.ports.idLoaded.send(id)}
    item.firstChild.onerror = function(){ app.ports.idLoaded.send(id)}
  } else if (id) {
    app.ports.idFailed.send(id);
  }
});


var winX = null;
var winY = null;
app.ports.disableScrolling.subscribe((msg) => {
  if (msg) {
      winX = window.scrollX;
      winY = window.scrollY;
  } else {
      winX = null;
      winY = null;
  }
});
window.addEventListener("scroll", function () {
  if (winX !== null && winY !== null) {
      window.scrollTo(winX, winY);
  }
});

var enableRecvScroll = true;
var ticking = 0;
window.addEventListener("scroll", function (event) {
  if (enableRecvScroll) {
    if (ticking > 5 || (document.documentElement.scrollTop == 0)) {
      window.requestAnimationFrame(function() {
        app.ports.recvScroll.send(document.documentElement.scrollTop);
        ticking = 0;
      });
    }
      ticking += 1;
  }
});

//storage
app.ports.save.subscribe((storage) => {
  localStorage.setItem("storage", JSON.stringify(storage));
  //app.ports.load.send(storage);
});

// Handle smoothly scrolling to links
const scrollToHash = () => {
  const BREAKPOINT_XL = 1920;
  //const NAVBAR_HEIGHT_PX = window.innerWidth > BREAKPOINT_XL ? 127 : 102;
  const element = window.location.hash && document.querySelector(window.location.hash);
  //localStorage.setItem("storage", localStorage.getItem("storage").replace('"mobileNav":true', '"mobileNav":false'));
  if (element) {
      // element.scrollIntoView({ behavior: 'smooth' })
      window.scroll({ behavior: "smooth", top: window.pageYOffset + element.getBoundingClientRect().top /*- NAVBAR_HEIGHT_PX*/ });
  } else {
      window.scroll({ behavior: "auto", top: 0 });
  }
};

app.ports.onUrlChange.subscribe((_) => {
  enableRecvScroll = false;
  ticking = 0
  setTimeout(scrollToHash, 400);
  enableRecvScroll = true;
});

function onSignIn(googleUser) {
  app.ports.google.send(googleUser.getAuthResponse().id_token)
}

app.ports.copyText.subscribe((text) => {
  if (window.clipboardData && window.clipboardData.setData) {
    // Internet Explorer-specific code path to prevent textarea being shown while dialog is visible.
    return window.clipboardData.setData("Text", text);

  }
  else if (document.queryCommandSupported && document.queryCommandSupported("copy")) {
      var textarea = document.createElement("textarea");
      textarea.textContent = text;
      textarea.style.position = "fixed";  // Prevent scrolling to bottom of page in Microsoft Edge.
      document.body.appendChild(textarea);
      textarea.select();
      try {
          return document.execCommand("copy");  // Security exception may be thrown by some browsers.
      }
      catch (ex) {
        app.ports.successfulCopy.send(false);
      }
      finally {
          document.body.removeChild(textarea);
          app.ports.successfulCopy.send(true);
      }
  }
});


app.ports.signOut.subscribe((_) => {
  var auth2 = gapi.auth2.getAuthInstance();
  auth2.signOut()
});
