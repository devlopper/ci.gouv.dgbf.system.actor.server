package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.IdentityBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.IdentityPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;

@ApplicationScoped
public class IdentityBusinessImpl extends AbstractBusinessEntityImpl<Identity, IdentityPersistence> implements IdentityBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Identity createFromInterface(Identity.Interface identity) {
		if(identity == null)
			return null;
		if(StringHelper.isBlank(identity.getFirstName()))
			throw new RuntimeException("Le nom est obligatoire");
		if(StringHelper.isBlank(identity.getLastNames()))
			throw new RuntimeException("Le prénom est obligatoire");
		if(StringHelper.isBlank(identity.getElectronicMailAddress()))
			throw new RuntimeException("L'adresse de courriel est obligatoire");
		Identity __identity__ = new Identity()
				.setActOfAppointmentReference(identity.getActOfAppointmentReference())
				.setActOfAppointmentSignatory(identity.getActOfAppointmentSignatory())
				.setActOfAppointmentSignatureDate(identity.getActOfAppointmentSignatureDate())
				.setActOfAppointmentSignatureDateAsTimestamp(identity.getActOfAppointmentSignatureDateAsTimestamp())
				.setAdministrativeFunction(identity.getAdministrativeFunction())
				.setAdministrativeUnit(identity.getAdministrativeUnit())
				.setCivility(identity.getCivility())
				.setElectronicMailAddress(identity.getElectronicMailAddress())
				.setFirstName(identity.getFirstName())
				.setGroup(identity.getGroup())
				.setIdentifier(identity.getElectronicMailAddress())
				.setLastNames(identity.getLastNames())
				.setMobilePhoneNumber(identity.getMobilePhoneNumber())
				.setNames(identity.getNames())
				.setOfficePhoneExtension(identity.getOfficePhoneExtension())
				.setOfficePhoneNumber(identity.getOfficePhoneNumber())
				.setPostalBoxAddress(identity.getPostalBoxAddress())
				.setRegistrationNumber(identity.getRegistrationNumber())
				;
		if(__identity__.getActOfAppointmentSignatureDate() == null) {
			if(__identity__.getActOfAppointmentSignatureDateAsTimestamp() != null)
				__identity__.setActOfAppointmentSignatureDate(LocalDate.ofInstant(Instant.ofEpochMilli(__identity__.getActOfAppointmentSignatureDateAsTimestamp())
						,ZoneId.systemDefault()));
		}
		create(__identity__);
		return __identity__;
	}
	
	@Override
	public String encryptElectroncicMailAddress(String electronicMailAddress) {
		if(StringHelper.isBlank(electronicMailAddress))
			throw new RuntimeException("Email à crypter pour URL est obligatoire");
		try {
			return encryptElectroncicMailAddress(electronicMailAddress, SECRET);
		} catch (Exception exception) {
			throw new RuntimeException(exception.toString());
		}
	}
	
	@Override
	public String decryptElectroncicMailAddress(String string) {
		if(StringHelper.isBlank(string))
			throw new RuntimeException("Email à décrypter à partir d'une URL est obligatoire");
		try {
			return decryptElectroncicMailAddress(string, SECRET);
		} catch (Exception exception) {
			throw new RuntimeException(exception.toString());
		}
	}
	
	/**/
	
	private static final String SECRET = "this_is_my_secret";
	public static final String ENCODING = "UTF-8";
	private static final String DIGEST_ALGORITHM = "SHA-1";
	private static final String CIPHER_ALGORITHM = "AES";	
	private static final String TRANSFORMATION = "AES/ECB/PKCS5Padding";
	
	private static SecretKeySpec SECRET_KEY;
    private static byte[] KEY;
 
    private static void setKey(String myKey) {
        MessageDigest sha = null;
        try {
        	KEY = myKey.getBytes(ENCODING);
            sha = MessageDigest.getInstance(DIGEST_ALGORITHM);
            KEY = sha.digest(KEY);
            KEY = Arrays.copyOf(KEY, 16); 
            SECRET_KEY = new SecretKeySpec(KEY, CIPHER_ALGORITHM);
        } 
        catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } 
        catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
    }
 
    private static String encryptElectroncicMailAddress(String string, String secret) throws Exception {
    	setKey(secret);
    	Cipher cipher = Cipher.getInstance(TRANSFORMATION);
    	cipher.init(Cipher.ENCRYPT_MODE, SECRET_KEY);
    	return Base64.getEncoder().encodeToString(cipher.doFinal(string.getBytes(ENCODING)));
    }
 
    private static String decryptElectroncicMailAddress(String string, String secret) throws Exception {
    	setKey(secret);
    	Cipher cipher = Cipher.getInstance(TRANSFORMATION);
    	cipher.init(Cipher.DECRYPT_MODE, SECRET_KEY);
    	return new String(cipher.doFinal(Base64.getDecoder().decode(string)));
    }
}