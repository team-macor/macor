use omg::Base;

fn main() -> anyhow::Result<()> {
    let mut ring = omg_crypt::RingBase::new();

    let msg = "HALLO".to_string();

    let key = ring.generate_sym_key();
    let encrypted = ring.symmetric_encrypt(&msg, &key)?;

    println!("Encrypted {:?}", encrypted);

    let dencrypted: String = ring.symmetric_dencrypt(&encrypted, &key)?;
    println!("Dencrypted {:?}", dencrypted);

    Ok(())
}
