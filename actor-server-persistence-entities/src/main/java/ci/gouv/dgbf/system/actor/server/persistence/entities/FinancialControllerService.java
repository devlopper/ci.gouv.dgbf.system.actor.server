package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Column;
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
@Entity @Table(name=FinancialControllerService.TABLE_NAME)
public class FinancialControllerService extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SECTION) private Section section;
	@Transient private String sectionAsString;
	@Transient private String sectionCode;
	
	@ManyToOne @JoinColumn(name = COLUMN_LOCALITY) private Locality locality;
	@Transient private String localityAsString;
	@Transient private String localityCode;
	
	@Column(name = COLUMN_ACTIVITY) private String activityIdentifier;
	
	@Override
	public FinancialControllerService setIdentifier(String identifier) {
		return (FinancialControllerService) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_SECTION = "section";
	public static final String FIELD_SECTION_AS_STRING = "sectionAsString";
	public static final String FIELD_SECTION_CODE = "sectionCode";
	
	public static final String FIELD_LOCALITY = "locality";
	public static final String FIELD_LOCALITY_AS_STRING = "localityAsString";
	public static final String FIELD_LOCALITY_CODE = "localityCode";
	
	public static final String FIELD_ACTIVITY_IDENTIFIER = "activityIdentifier";
	
	public static final String TABLE_NAME = "SERVICE_CF";
	
	public static final String COLUMN_SECTION = "SECTION";
	public static final String COLUMN_LOCALITY = "LOCALITE";
	public static final String COLUMN_ACTIVITY = "ACTIVITE";
	
	public static final String IDENTIFIER_INSTITUTIONS = "CFINSTITUTIONS";
	
	public static final String CODE_INSTITUTIONS = "CFINSTITUTIONS";
}