package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.time.LocalDateTime;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=RejectedAccountRequest.TABLE_NAME)
@Access(AccessType.FIELD)
public class RejectedAccountRequest extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_FIRST_NAME) @NotNull private String firstName;
	@Column(name = COLUMN_LAST_NAMES) @NotNull private String lastNames;
	@Column(name = COLUMN_ELECTRONIC_MAIL_ADDRESS) @NotNull private String electronicMailAddress;
	@Column(name = COLUMN_REASON) private String reason;
	@Column(name = COLUMN_REQUEST_DATE) private @NotNull LocalDateTime requestDate;
	@Column(name = COLUMN_DATE) private @NotNull LocalDateTime date;
	
 	@Transient private String names;
	
	@Override
	public RejectedAccountRequest setIdentifier(String identifier) {
		return (RejectedAccountRequest) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_FIRST_NAME = "firstName";
	public static final String FIELD_LAST_NAMES = "lastNames";
	public static final String FIELD_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	public static final String FIELD_REASON = "reason";
	public static final String FIELD_REQUEST_DATE = "requestDate";
	public static final String FIELD_DATE = "date";
	public static final String FIELD_NAMES = "names";
	
	public static final String TABLE_NAME = "DEMANDE_COMPTE_REJETEE";	
	
	public static final String COLUMN_FIRST_NAME = "NOM";
	public static final String COLUMN_LAST_NAMES = "PRENOMS";
	public static final String COLUMN_ELECTRONIC_MAIL_ADDRESS = "EMAIL";
	public static final String COLUMN_REASON = "MOTIF";
	public static final String COLUMN_REQUEST_DATE = "DATE_DEMANDE";
	public static final String COLUMN_DATE = "DATE_REJET";
}