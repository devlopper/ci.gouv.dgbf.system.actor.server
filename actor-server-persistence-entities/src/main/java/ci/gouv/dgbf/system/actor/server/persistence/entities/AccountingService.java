package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=AccountingService.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_WRITE)
public class AccountingService extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_LOCALITY) private Locality locality;
	@Transient private String localityAsString;
	@Transient private String localityCode;
	
	@Override
	public AccountingService setIdentifier(String identifier) {
		return (AccountingService) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_LOCALITY = "locality";
	public static final String FIELD_LOCALITY_AS_STRING = "localityAsString";
	public static final String FIELD_LOCALITY_CODE = "localityCode";
	
	public static final String TABLE_NAME = "SERVICE_CPT";	
	
	public static final String COLUMN_LOCALITY = "LOCALITE";
}