use omg::Base;

fn main() -> anyhow::Result<()> {
    let mut ring = omg_crypt::RingBase::new();

    let msg = "HALLO".to_string();

    let key = ring.gen_sym_key();
    let encrypted = ring.sym_encrypt(&msg, &key)?;

    println!("Encrypted {:?}", encrypted);

    let decrypted: String = ring.sym_decrypt(&encrypted, &key)?;
    println!("decrypted {:?}", decrypted);

    Ok(())
}
