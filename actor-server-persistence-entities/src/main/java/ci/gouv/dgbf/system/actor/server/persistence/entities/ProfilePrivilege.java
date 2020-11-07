package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Column;
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
@Entity @Table(name=ProfilePrivilege.TABLE_NAME)
public class ProfilePrivilege extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_PROFILE) @NotNull private Profile profile;
	@ManyToOne @JoinColumn(name = COLUMN_PRIVILEGE) @NotNull private Privilege privilege;
	@Column(name = COLUMN_VISIBLE) private Boolean visible;
	
	@Override
	public ProfilePrivilege setIdentifier(String identifier) {
		return (ProfilePrivilege) super.setIdentifier(identifier);
	}
	
	public ProfilePrivilege setProfileFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setProfile(null);
		else
			setProfile(EntityFinder.getInstance().find(Profile.class, identifier));
		return this;
	}
	
	public ProfilePrivilege setPrivilegeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setPrivilege(null);
		else
			setPrivilege(EntityFinder.getInstance().find(Privilege.class, identifier));
		return this;
	}
	
	public static final String FIELD_PROFILE = "profile";
	public static final String FIELD_PRIVILEGE = "privilege";
	public static final String FIELD_VISIBLE = "visible";
	
	public static final String COLUMN_PROFILE = "profile";
	public static final String COLUMN_PRIVILEGE = "privilege";
	public static final String COLUMN_VISIBLE = "visible";
	
	public static final String TABLE_NAME = "PROFILE_PRIVILEGE";	
}