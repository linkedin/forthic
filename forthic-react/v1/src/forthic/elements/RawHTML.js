import sanitizeHtml from 'sanitize-html'

function RawHTML({html}) {
    return <div dangerouslySetInnerHTML={{__html: sanitizeHtml(html, {
        allowedAttributes: false
    })}} />;
}

export default RawHTML;
