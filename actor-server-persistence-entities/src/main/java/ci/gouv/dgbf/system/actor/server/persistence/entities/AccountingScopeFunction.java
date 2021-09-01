package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=AccountingScopeFunction.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
public class AccountingScopeFunction extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {

	@Column(name = COLUMN_TRESOR_IDENTIFIER)
	private String tresorIdentifier;
	
	public static final String FIELD_TRESOR_IDENTIFIER = "tresorIdentifier";
	
	public static final String TABLE_NAME = "TA_POSTE_COMPTABLE";	
	
	public static final String COLUMN_TRESOR_IDENTIFIER = "IDENTIFIANT_TRESOR";
	
}