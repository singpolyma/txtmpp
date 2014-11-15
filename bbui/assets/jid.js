function toBare(jid) {
	return jid.split('/', 2)[0];
}

function localpart(jid) {
	return jid.split('@', 2)[0];
}

function domainpart(jid) {
	return toBare(jid).split('@', 2)[1];
}

function resourcepart(jid) {
	return jid.split('/', 2)[1];
}
