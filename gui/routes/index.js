const { exec, spawn } = require('child_process');
const fs = require('fs');
const path = require('path');

const csv = require('csv-parser');
const find = require('find-process');
const fkill = require('fkill');
const untildify = require('untildify');

const express = require('express');
const router = express.Router();

const schismaBin = 'bin/schisma';

router.get('/', function(req, res, next) {
  const command = `${schismaBin} synth parameters`;
  const options = { cwd: path.resolve('../') };

  exec(command, options, function(error, stdout, stderr) {
    res.render('index', {
      defaultSynthParameters: stdout,
      title: 'Schisma'
    });
  })
});

router.get('/instruments', function(req, res, next) {
  const file = untildify(req.query.file);
  const contents = JSON.parse(fs.readFileSync(file, 'utf8'));
  res.json(contents.instruments);
});

router.put('/instruments', function(req, res, next) {
  const trackerFile = path.resolve(untildify(req.body.instrumentsFile));
  const contents = JSON.parse(req.body.contents);
  const output = JSON.stringify(contents, null, 2);

  fs.writeFile(trackerFile, output, function(error) {
    if (error) {
      res.sendStatus(400);
    } else {
      res.sendStatus(200);
    }
  });
});

router.put('/play', function(req, res, next) {
  fkill('csound', { silent: true });

  const trackerFile = path.resolve(untildify(req.body.trackerFile));
  const instrumentsFile = path.resolve(untildify(req.body.instrumentsFile));

  const schisma = spawn(schismaBin, [
    'tracker',
    'play',
    '-t', trackerFile,
    '-i', instrumentsFile,
    '-s', req.body.start,
    '-e', req.body.end,
  ], {
    cwd: path.resolve('../'),
    detached: true,
    stdio: 'ignore'
  })

  schisma.unref();


  let iterations = 10;

  const findProcess = function() {
    find('name', 'csound', true).then(function (list) {
      if (list.length == 0) {
        if (iterations <= 1) {
          res.sendStatus(500);
        } else {
          iterations = iterations - 1;
          setTimeout(findProcess, 300);
        }
      } else {
        res.sendStatus(202);
      }
    });
  }

  findProcess();
});

router.put('/stop', function(req, res, next) {
  fkill('csound', { silent: true });

  res.sendStatus(204);
});

router.get('/tracker', function(req, res, next) {
  const results = [];
  const csvOptions = {
    headers: false,
    mapValues: ({ header, index, value }) => value.trim()
  };
  const file = untildify(req.query.file);

  // TODO: Check for existence of file

  fs.createReadStream(file)
    .pipe(csv(csvOptions))
    .on('data', (data) => results.push(Object.values(data)))
    .on('end', () => {
      res.json(results);
    });
});

router.put('/tracker', function(req, res, next) {
  const trackerFile = path.resolve(untildify(req.body.trackerFile));

  fs.writeFile(trackerFile, req.body.contents, function(error) {
    if (error) {
      res.sendStatus(400);
    } else {
      res.sendStatus(200);
    }
  });
});


module.exports = router;
