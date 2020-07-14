package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=AccountRequest.TABLE_NAME)
public class AccountRequest extends AbstractIdentifiableSystemScalarStringImpl implements Identity.Interface,Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_IDENTITY) private Identity identity;
	
	@Transient private String firstName;
	@Transient private String lastNames;
	@Transient private String electronicMailAddress;	
	@Transient private String names;
	
	@Override
	public AccountRequest setIdentifier(String identifier) {
		return (AccountRequest) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_IDENTITY = "identity";
	public static final String FIELD_FIRST_NAME = "firstName";
	public static final String FIELD_LAST_NAMES = "lastNames";
	public static final String FIELD_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	public static final String FIELD_NAMES = "names";
	
	public static final String TABLE_NAME = "DEMANDE_COMPTE";
	
	public static final String COLUMN_IDENTITY = "IDENTITE";
}