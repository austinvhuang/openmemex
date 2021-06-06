import transformers
from transformers import AutoTokenizer, T5ForConditionalGeneration
import pprint
from typing import Any
import torch


def export(model_name, example):
    pp = pprint.PrettyPrinter(indent=4, compact=True, width=120)

    model = T5ForConditionalGeneration.from_pretrained(model_name, torchscript=True)
    model.eval()

    tokenizer = AutoTokenizer.from_pretrained(model_name)

    print("Encoder ====")

    tokenized_inputs = tokenizer([example], padding="longest", return_tensors="pt")
    pp.pprint(tokenized_inputs)
    back_decoded = tokenizer.batch_decode(
        tokenized_inputs["input_ids"], skip_special_tokens=False
    )
    pp.pprint({"back_decoded": back_decoded})

    generated_ids = model.generate(
        input_ids=tokenized_inputs.input_ids,
        attention_mask=tokenized_inputs.attention_mask,
        num_beams=1,
        max_length=512,
        do_sample=False,
        repetition_penalty=1,
        no_repeat_ngram_size=0,
        length_penalty=1,
    )

    print("\nCheck Decoder ====\n")

    decoded = tokenizer.batch_decode(generated_ids, skip_special_tokens=False)

    tokenized_decoder_inputs = tokenizer(
        # because the model doesn't shift right unless labels are passed
        "<pad> " + decoded[0],
        padding="longest",
        return_tensors="pt",
    )

    pp.pprint(tokenized_decoder_inputs)
    back_decoded = tokenizer.batch_decode(
        tokenized_decoder_inputs["input_ids"], skip_special_tokens=False
    )
    pp.pprint({"back_decoded": back_decoded})

    print("\nExporting ====\n")

    traced = torch.jit.trace(
        model,
        [
            tokenized_inputs["input_ids"],
            tokenized_inputs.attention_mask,
            tokenized_decoder_inputs["input_ids"],
            tokenized_decoder_inputs["attention_mask"],
        ],
    )

    torch.save(
        {
            "input_ids": tokenized_inputs["input_ids"],
            "attention_mask_inputs": tokenized_inputs.attention_mask,
            "decoder_input_ids": tokenized_decoder_inputs["input_ids"],
            "attention_mask_decoder": tokenized_decoder_inputs["attention_mask"],
        },
        "traced_" + model_name + ".example.pt",
    )

    # print(traced.graph)

    torch.jit.save(traced, "traced_" + model_name + ".pt")


example_bank = {
    "qnli": "qnli question: Where did Jebe die? sentence: Genghis Khan recalled Subutai back to Mongolia soon afterwards, and Jebe died on the road back to Samarkand.",
    "qa1": "question: What does increased oxygen concentrations in the patient’s lungs displace? context: Hyperbaric (high-pressure) medicine uses special oxygen chambers to increase the partial pressure of O 2 around the patient and, when needed, the medical staff. Carbon monoxide poisoning, gas gangrene, and decompression sickness (the ’bends’) are sometimes treated using these devices. Increased O 2 concentration in the lungs helps to displace carbon monoxide from the heme group of hemoglobin. Oxygen gas is poisonous to the anaerobic bacteria that cause gas gangrene, so increasing its partial pressure helps kill them. Decompression sickness occurs in divers who decompress too quickly after a dive, resulting in bubbles of inert gas, mostly nitrogen and helium, forming in their blood. Increasing the pressure of O 2 as soon as possible is part of the treatment.",
    "qa2": "question: What is Joe's mother's name? context: Joe and Jenny are married. Jenny's mom is Jean. Joe's mom is Jennifer. Their son is Bob.",
    "summarization1:": "summarize: Transfer learning, where a model is first pre-trained on a data-rich task before being fine-tuned on a downstream task, has emerged as a powerful technique in natural language processing (NLP).",
    "translation1": "translate English to German: Transfer learning, where a model is first pre-trained on a data-rich task before being fine-tuned on a downstream task, has emerged as a powerful technique in natural language processing (NLP).",
}

if __name__ == "__main__":

    export("t5-small", example_bank["qa1"])
