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

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    apiEndpoint: apiEndpoint(),
    apiAuth: process.env.ELM_APP_API_AUTH,
    time: Date.now()
  }
});

registerServiceWorker();
