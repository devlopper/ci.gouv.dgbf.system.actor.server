package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Exercise.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
@org.hibernate.annotations.Immutable
public class Exercise extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Exercise setIdentifier(String identifier) {
		return (Exercise) super.setIdentifier(identifier);
	}
	
	public static final String TABLE_NAME = "VM_APP_EXERCICE";	
}