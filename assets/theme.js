(() => {
  function getUtterancesTheme(mode) {
    return mode === 'dark' ? 'github-dark' : 'github-light';
  }

  // Defer loading utterances until everything else is loaded.
  // This way more important page content is prioritized.
  window.addEventListener('load', () => {
    let initialTheme = 'github-light';
    const toggle = document.getElementsByTagName('dark-mode-toggle');
    if (toggle.length !== 1) {
      console.warn('Expected one dark-mode-toggle, but got: ', toggle);
    } else {
      initialTheme = getUtterancesTheme(toggle[0].mode);
    }

    // Add script that loads utterance
    const commentsContainer = document.getElementById('comments');
    if (commentsContainer) {
      const s = document.createElement('script');
      s.src = 'https://utteranc.es/client.js';
      s.setAttribute('repo', 'mgsloan/mgsloan-site');
      s.setAttribute('issue-term', 'pathname');
      s.setAttribute('label', 'comments');
      s.setAttribute('theme', initialTheme);
      s.setAttribute('crossorigin', 'anonymous');
      s.setAttribute('async', '');
      commentsContainer.appendChild(s);
    }
  });

  document.addEventListener('colorschemechange', (e) => {
    const mode = e.detail.colorScheme;
    const theme = getUtterancesTheme(mode);
    document.documentElement.classList.toggle('dark', mode === 'dark');
    for (const frame of document.getElementsByClassName('utterances-frame')) {
      frame.contentWindow.postMessage({ type: 'set-theme', theme:  theme },  '*');
    }
  });
})();
