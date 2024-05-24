import express from 'express';
import OpenAI from 'openai';

const openai = new OpenAI(process.env.OPENAI_API_KEY);
const app = express();
const port = 5040;

app.use(express.json());

app.post('/transcript', async (req, res) => {
  try {
    const transcript = req.body.data;
    console.log("Received transcript", transcript);

    const response = await openai.chat.completions.create({
      messages: [
        { role: 'system', content: 'You are a helpful assistant tasked with repairing errors in speech-to-text transcription. Users send you the original transcription and you respond with the correction, nothing else.' },
        { role: 'user', content: transcript }
      ],
      model: 'gpt-4o',
      max_tokens: 256,
    });
    await console.log("Corrected transcript:", response.choices[0].message.content)
    await res.send(response)
  } catch (error) {
    console.error(error);
    res.status(500).send(`An error occurred: ${error}`);
  }
});

app.post('/script', async (req, res) => {
  try {
    const query = req.body.data;
    console.log("Received query", query);

    const response = await openai.chat.completions.create({
      messages: [
        { role: 'system', content: 'You are an expert programmer. Your task is to write an executable snippet of code implementing the user query, nothing else' },
        { role: 'user', content: query }
      ],
      model: 'gpt-4o',
      max_tokens: 1024,
    })
    await console.log("Script generated:", response.choices[0].message.content)
    await res.send(response)
  } catch (error) {
    console.error(error);
    res.status(500).send(`An error has occured: ${error}`)
  }
});

app.post('/question', async (req, res) => {
  try {
    const question = req.body.data;
    console.log("Received question", query);

    const response = await openai.chat.completions.create({
      messages: [
        { role: 'system', content: 'You are general assistant, expert in computer science, mathematics and natural language recognition. Your task is to answer a technical question to the best of your abilities. Respond with technical terms, as if talking to a collegue. In particular, avoid politeness figures and stylistic elements like introductory or conclusion paragraphs' },
        { role: 'user', content: question }
      ],
      model: 'gpt-4o',
      max_tokens: 1024,
    })
    await console.log("Script generated:", response.choices[0].message.content)
    await res.send(response)
  } catch (error) {
    console.error(error);
    res.status(500).send(`An error has occured: ${error}`)
  }
});

app.listen(port, () => {
  console.log(`Server is running at http://localhost:${port}`);
});
