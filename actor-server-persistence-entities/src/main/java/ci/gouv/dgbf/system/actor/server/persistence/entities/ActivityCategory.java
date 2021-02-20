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
@Entity @Table(name=ActivityCategory.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
@org.hibernate.annotations.Immutable
public class ActivityCategory extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public ActivityCategory setIdentifier(String identifier) {
		return (ActivityCategory) super.setIdentifier(identifier);
	}
	
	@Override
	public ActivityCategory setCode(String code) {
		return (ActivityCategory) super.setCode(code);
	}
	
	@Override
	public ActivityCategory setName(String name) {
		return (ActivityCategory) super.setName(name);
	}
	
	@Override
	public String toString() {
		return code+" "+name;
	}
	
	public static final String TABLE_NAME = "VM_APP_CATEG_ATV";	
}