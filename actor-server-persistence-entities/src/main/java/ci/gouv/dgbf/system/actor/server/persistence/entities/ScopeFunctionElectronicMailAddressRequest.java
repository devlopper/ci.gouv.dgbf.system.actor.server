package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ScopeFunctionElectronicMailAddressRequest.TABLE_NAME)
public class ScopeFunctionElectronicMailAddressRequest extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private String electronicMailAddress;
	
	@Override
	public ScopeFunctionElectronicMailAddressRequest setIdentifier(String identifier) {
		return (ScopeFunctionElectronicMailAddressRequest) super.setIdentifier(identifier);
	}
	
	public static final String TABLE_NAME = "VMA_POSTE_EMAIL_DEMANDE";
	
	public static final String FIELD_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
}