import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const apiEndpoint = () => {
  if (/localhost|127\.0\.0\.1|::1/i.test(location.hostname)) {
    return "https://www.toggl.com/api/v8/"
  } else {
    return "/api/"
  }

}

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    apiEndpoint: apiEndpoint(),
    apiAuth: process.env.ELM_APP_API_AUTH,
    timezone: process.env.ELM_APP_TIMEZONE,
    time: Date.now()
  }
});

app.ports.favicon.subscribe(message => {
  var favicon = document.getElementById('favicon');
  switch (message) {
    case "Running":
      favicon.href = favicon.href.replace("stop", "run");
      break;
    case "Stopped":
      favicon.href = favicon.href.replace("run", "stop");
      break;
    default:
      return;
  }
});

registerServiceWorker();
