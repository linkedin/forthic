import sanitizeHtml from 'sanitize-html'

function RawHTML({html}) {
    return <div dangerouslySetInnerHTML={{__html: sanitizeHtml(html)}} />;
}

export default RawHTML;
