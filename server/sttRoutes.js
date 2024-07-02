import express from "express";
const router = express.Router();
import Anthropic from "@anthropic-ai/sdk";

const anthropic = new Anthropic({
  api_key: process.env["ANTHROPIC_API_KEY"]
});

router.post('/review-transcription', async (req, res) => {
  try {
    const {
      input,
      temperature,
      maxTokens
    } = {
      temperature: 0.2,
      maxTokens: 250,
      ...req.body.data
    };

    await console.log(`/review-transcript:Request: {req}`);

    const response = await anthropic.messages.create({
      model: "claude-3-haiku-20240307",
      max_tokens: maxTokens,
      temperature,
      system: "You are a helpful assistant. Your task is to correct any spelling discrepancies in the transcribed text. Only add necessary punctuation such as periods, commas, and capitalization, and use only the context provided.",
      messages: [
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": input
            }
          ]
        }
      ]
    });

    await console.log(`/review-transcript:Response: ${response}`)
    await res.status(200).send({data: response.content[0].text})
  } catch (error) {
    await console.error(error);
    await res.status(500).send(`An error occurred: ${error}`);
    await console.error(`/correct-transcript:Error: ${error}`)
  }
});

export { router as sttRouter };
