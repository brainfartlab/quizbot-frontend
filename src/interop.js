export const flags = ({ env }) => {
  var profile = localStorage.profile
  var token = localStorage.token
  console.log("Loading...")
  console.log(env)

  return {
    user: (profile && token) ? {
      profile: JSON.parse(profile),
      token,
    } : null,
    backendUri: env.QUIZ_API_URI,
  }
}

export const onReady = ({ env, app }) => {
  var webAuth = new auth0.WebAuth({
    domain: env.AUTH0_TENANT,
    clientID: env.AUTH0_CLIENT_ID,
    responseType: 'token',
    redirectUri: env.URL,
  });

  if (app.ports && app.ports.auth0authorize) {
    app.ports.auth0authorize.subscribe((options) => {
      webAuth.authorize({
        audience: 'https://auth0-jwt-authorizer',
        // redirectUri: `http://localhost:1234${options.redirectPath}`
      })
    })
  }

  if (app.ports && app.ports.auth0logout) {
    app.ports.auth0logout.subscribe(() => {
      localStorage.removeItem('profile')
      localStorage.removeItem('token')
    })
  }

  webAuth.parseHash({ hash: window.location.hash }, (err, authResult) => {
    if (err) {
      return console.error(err)
    }

    if (authResult) {
      webAuth.client.userInfo(authResult.accessToken, (err, profile) => {
        var result = { err: null, ok: null }
        var token = authResult.accessToken

        if (err) {
          result.err = err.details
          result.err.name = result.err.name ? result.err.name : null;
          result.err.code = result.err.code ? result.err.code : null;
          result.err.statusCode = result.err.statusCode ? result.err.statusCode : null;
        }

        if (authResult) {
          result.ok = { profile: profile, token: token }
          localStorage.setItem('profile', JSON.stringify(profile));
          localStorage.setItem('token', token);
        }

        app.ports.auth0authResult.send(result);
      })

      window.location.hash = ''
    }
  })
}
