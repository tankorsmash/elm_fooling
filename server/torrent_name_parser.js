var tnp = require('torrent-name-parser');


const myArgs = process.argv.slice(2);

if (myArgs.length == 0) {
	process.stderr.write(JSON.stringify({
		success:false,
		message:"requires a single torrent name as an arg"
	}, null, 4));
	process.exit();
}

process.stdout.write(JSON.stringify({
	success:true,
	result: tnp(myArgs[0])
}, null, 4));
