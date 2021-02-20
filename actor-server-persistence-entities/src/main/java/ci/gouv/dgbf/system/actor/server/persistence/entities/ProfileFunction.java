package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ProfileFunction.TABLE_NAME)
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_WRITE)
public class ProfileFunction extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_PROFILE) @NotNull private Profile profile;
	@ManyToOne @JoinColumn(name = COLUMN_FUNCTION) @NotNull private Function function;
	
	@Override
	public ProfileFunction setIdentifier(String identifier) {
		return (ProfileFunction) super.setIdentifier(identifier);
	}
	
	public ProfileFunction setProfileFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setProfile(null);
		else
			setProfile(EntityFinder.getInstance().find(Profile.class, identifier));
		return this;
	}
	
	public ProfileFunction setFunctionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setFunction(null);
		else
			setFunction(EntityFinder.getInstance().find(Function.class, identifier));
		return this;
	}
	
	public static final String FIELD_PROFILE = "profile";
	public static final String FIELD_FUNCTION = "function";
	
	public static final String COLUMN_PROFILE = "profile";
	public static final String COLUMN_FUNCTION = "fonction";
	
	public static final String TABLE_NAME = "PROFILE_FONCTION";	
}