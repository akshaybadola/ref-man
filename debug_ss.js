const CDP = require('chrome-remote-interface');

async function get_ss_url_from_chrome() {
    let client;
    try {
        // connect to endpoint
        client = await CDP();
        // extract domains
        const {Network, Page} = client;
        // setup handlers
        Network.requestWillBeSent((params) => {
            if (params.request.url.toLowerCase().startsWith("https://www.semanticscholar.org/api/1/search")){
                console.log(JSON.stringify(params));
            }
        });
        // enable events then start!
        await Network.enable();
        await Page.enable();
        await Page.navigate({url: 'https://semanticscholar.org/search?q=A%20fast%20and%20accurate%20one-stage%20approach%20to%20visual%20grounding'});
        await Page.loadEventFired();
    } catch (err) {
        console.error(err);
    } finally {
        if (client) {
            await client.close();
        }
    }
}

get_ss_url_from_chrome();
