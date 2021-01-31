package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Profile.TABLE_NAME)
public class Profile extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_TYPE) @NotNull private ProfileType type;
	
	@Transient private Collection<Privilege> privileges;
	@Transient private Collection<String> privilegesAsStrings;
	
	@Override
	public Profile setIdentifier(String identifier) {
		return (Profile) super.setIdentifier(identifier);
	}
	
	@Override
	public Profile setCode(String code) {
		return (Profile) super.setCode(code);
	}
	
	@Override
	public Profile setName(String name) {
		return (Profile) super.setName(name);
	}
	
	public Profile setTypeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setType(null);
		else
			setType(EntityFinder.getInstance().find(ProfileType.class, identifier));
		return this;
	}

	public Collection<Privilege> getPrivileges(Boolean injectIfNull) {
		if(privileges == null && Boolean.TRUE.equals(injectIfNull))
			privileges = new ArrayList<>();
		return privileges;
	}
	
	public Profile addPrivilegesByIdentifiers(Collection<String> identifiers) {
		if(CollectionHelper.isEmpty(identifiers))
			return this;
		identifiers.forEach(identifier -> {
			if(StringHelper.isNotBlank(identifier)) {
				Privilege privilege = EntityFinder.getInstance().find(Privilege.class, identifier);
				if(privilege != null)
					getPrivileges(Boolean.TRUE).add(privilege);
			}				
		});
		return this;
	}
	
	public Profile addPrivilegesByIdentifiers(String...identifiers) {
		if(ArrayHelper.isEmpty(identifiers))
			return this;
		return addPrivilegesByIdentifiers(CollectionHelper.listOf(identifiers));
	}
	
	public static final String FIELD_TYPE = "type";
	public static final String FIELD_PRIVILEGES = "privileges";
	public static final String FIELD_PRIVILEGES_AS_STRINGS = "privilegesAsStrings";
	
	public static final String COLUMN_TYPE = "type";
	
	public static final String TABLE_NAME = "PROFILE";
	
	public static final String CODE_UTILISATEUR = "UTILISATEUR";
	public static final String CODE_ADMINISTRATEUR = "ADMINISTRATEUR";
	public static final String CODE_CHARGE_ETUDE_DAS = "CEDAS";
}