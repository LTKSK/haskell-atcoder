# setup

- pyenvで3.11.9のvenv作る
- activateして、pip install online-judge-tools 
- oj-setup.sh {問題のURL}
- ojth（自前alias）でtest
- ojs でsubmit

# memo

- ガード節はboolで分岐する場合
- caseは値で分岐する場合。配列要素の3番目がほにゃららとかでも捕まえられてよい
- catMaybes [m1, m2, m3]でモナドを繋げることができる。選んだ時、選ばなかった時をJust/Nothingで表現してその組み合わせ一覧を得る
