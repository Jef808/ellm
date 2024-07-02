import {exec} from 'child_process';
import express from 'express';
import fs from 'fs';
import path from 'path';
import { sttRouter } from './sttRoutes';
import OpenAI from 'openai';

const openai = new OpenAI(process.env.OPENAI_API_KEY);
const app = express();

app.use(express.json());

app.use('/', sttRouter);

async function generateSpeech(input, speechFile) {
  try {
    const mp3 = await openai.audio.speech.create({
      model: "tts-1",
      voice: "alloy",
      input: input,
    });

    console.log(speechFile);
    const buffer = Buffer.from(await mp3.arrayBuffer());
    await fs.promises.writeFile(speechFile, buffer);
    return speechFile;
  } catch (error) {
    console.error(`Error generating speech: ${error.message}`);
  }
}

app.post('/tts', async (req, res) => {
  let {input, filepath} = req.body.data;
  console.log("Received tts request with input", input);

  if (!input) {
    res.status(400).send("No input provided");
    return;
  }

  if (!filepath) {
    filepath = '/tmp/speech.mp3';
  }

  const speechFile = path.resolve(filepath);
  try {
    const file = await generateSpeech(input, speechFile);
    console.log(`Speech saved at ${speechFile}`);
    res.status(200).send(`Speech saved at ${speechFile}`);
  } catch (error) {
    console.error('Error generating speech:', error);
    res.status(500).send("Error generating speech");
  }
});

app.post('/script', async (req, res) => {
  try {
    const {input} = req.body.data;
    console.log("Received script request with input", input);

    const response = await openai.chat.completions.create({
      messages: [
        { role: 'system', content: 'You are an expert python programmer. Your task is to write an executable snippet of code implementing the user query, nothing else.' },
        { role: 'user', content: `## Query:\n${input}\n\n## Code:\n\`\`\`python\n` }
      ],
      stop: ['```\n'],
      model: 'gpt-4o',
      max_tokens: 1024,
    })
    await console.log("Script generated:", response.choices[0].message.content)
    await res.status(200).send(response)
  } catch (error) {
    console.error(error);
    res.status(500).send(`An error has occured: ${error}`)
  }
});

app.post('/snippet', async (req, res) => {
  try {
    const {input} = req.body.data;
    console.log("Received code snippet request with input", input);

    const response = await openai.chat.completions.create({
      messages: [
        { role: 'system', content: 'You are an expert programmer. Your task is to write a snippet of code implementing the user query, nothing else.' },
        { role: 'user', content: `## Query:\n${input}\n\n## Code:\n\`\`\`python\n` }
      ],
      stop: ['```\n'],
      model: 'gpt-4o',
      max_tokens: 1024,
    })
    await console.log("Snippet generated:", response.choices[0].message.content)
    await res.status(200).send(response)
  } catch (error) {
    console.error(error);
    res.status(500).send(`An error has occured: ${error}`)
  }
});

app.post('/question', async (req, res) => {
  try {
    const {input} = req.body.data;
    console.log("Received question/answer request with input", input);

    const response = await openai.chat.completions.create({
      messages: [
        { role: 'system', content: 'You are general assistant, expert in computer science, mathematics and natural language recognition. Your task is to answer a technical question to the best of your abilities. Respond with your full technical expertise, as if responding to another expert. Avoid politeness figures and stylistic elements like introductory or conclusion paragraphs.' },
        { role: 'user', content: input }
      ],
      model: 'gpt-4o',
      max_tokens: 1024,
    })
    await console.log("Anwer generated:", response.choices[0].message.content)
    await res.status(200).send(response)
  } catch (error) {
    console.error(error);
    res.status(500).send(`An error has occured: ${error}`)
  }
});

const getCommandLineArgs = () => {
  const args = process.argv.slice(2);
  const argMap = {};

  for (let i = 0; i < args.length; i += 2) {
    const key = args[i];
    const value = args[i + 1];
    if (!value || value.startsWith('--')) {
      throw new Error(`Missing value for argument ${key}`);
    }
    argMap[key] = value;
  }

  if (!argMap['--host'] || !argMap['--port']) {
    throw new Error('Both --host and --port arguments are required');
  }

  return argMap;
};

const args = getCommandLineArgs();
const host = args['--host'];
const port = args['--port'];

app.listen(port, host, () => {
  console.log(`ELLM Server listening at http://${host}:${port}`);
});
