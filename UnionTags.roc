interface UnionTags
    exposes [
        unionTags
    ]

    imports [
        Core.{Json}
    ]

UnionTags := {other:Json}
    implements [
        EncoderFormatting {
            u8: encodeU8,
            u16: encodeU16,
            u32: encodeU32,
            u64: encodeU64,
            u128: encodeU128,
            i8: encodeI8,
            i16: encodeI16,
            i32: encodeI32,
            i64: encodeI64,
            i128: encodeI128,
            f32: encodeF32,
            f64: encodeF64,
            dec: encodeDec,
            bool: encodeBool,
            string: encodeString,
            list: encodeList,
            record: encodeRecord,
            tuple: encodeTuple,
            tag: encodeTag,
        },
    ]

unionTags = @UnionTags {other:Core.json }

encodeTag:Str, List (Encoder _) -> Encoder _
encodeTag = \name, encoders ->
    Encode.custom \bytes, @UnionTags{other}->
        when encoders is
            [only] ->
                bytes |> Encode.appendWith only  (@UnionTags {other})
            _-> crash "cannot encode multi arg tags as unions "

# encodeTag:Str, List (Encoder _) -> Encoder _
# encodeTag=\name,encoders->
#     Encode.custom \bytes, @UnionTags{}->
#         bytes

forward=\n->
    Encode.custom \bytes, @UnionTags {other} ->
        bytes |>Encode.append n other

# all the other functions just forward 


encodeU8 = forward
 
encodeU16 = forward

encodeU32 = forward

encodeU64 =forward

encodeU128 =forward

encodeI8 =forward

encodeI16 =forward

encodeI32 =forward

encodeI64 =forward

encodeI128 =forward

encodeF32 =forward

encodeF64 =forward

encodeDec =forward

encodeBool = forward
encodeString = forward

toJson=\encoder->
    Encode.custom \bytes, other->
        bytes|> Encode.appendWith encoder (@UnionTags{other})
fromJson=\encoder->
    Encode.custom \bytes, @UnionTags{other}->
        bytes|> Encode.appendWith  encoder (other)

encodeRecord:List { key : Str, value : Encoder _} -> Encoder _
encodeRecord = \fields ->
        fields|>List.map (\{key, value} -> {key,value:(toJson value)})|> Encode.record|>fromJson

encodeTuple: List (Encoder _) -> Encoder _
encodeTuple = \elems ->
    elems|>List.map toJson|> Encode.tuple|>fromJson
    

encodeList:List elem, (elem -> Encoder _) -> Encoder _
encodeList = \lst,encodeElem ->
    newEncode=\elem->encodeElem elem|>toJson
    lst|>Encode.list newEncode|>fromJson



# encodeTag = \name,encoders->
#     Encode.custom \bytes, @UnionTags {} ->
#         List.concat bytes []

# This type should in theory allow us to wrap any tag union in this special encoding and decoding like: `UnionTagsType[MyTag MyType]` 
UnionTagsType a:= []a where a implements  Encoding 
     implements [ Encoding { toEncoder: toUnionEncoder}]
toUnionTags=\val-> @UnionTagsType val
fromUnionTags=\@UnionTagsType val-> val

toUnionEncoder:UnionTagsType a -> Encoder _
toUnionEncoder=\@UnionTagsType val-> 
    Encode.custom \bytes, fmt->
        bytes|>Encode.append val unionTags



expect
    test:UnionTagsType[MyTag {val:Str}] 
    test= toUnionTags (MyTag {val:"hi"})
    val=test|>Encode.toBytes (Core.json)|>Str.fromUtf8

    val==Ok """{"val":"hi"}"""


    

