package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Privilege.TABLE_NAME)
public class Privilege extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_TYPE) @NotNull private PrivilegeType type;
	@Column(name = COLUMN_PARENT_IDENTIFIER) private String parentIdentifier;
	
	@Override
	public Privilege setIdentifier(String identifier) {
		return (Privilege) super.setIdentifier(identifier);
	}
	
	@Override
	public Privilege setCode(String code) {
		return (Privilege) super.setCode(code);
	}
	
	@Override
	public Privilege setName(String name) {
		return (Privilege) super.setName(name);
	}
	
	public Privilege setTypeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setType(null);
		else
			setType(EntityFinder.getInstance().find(PrivilegeType.class, identifier));
		return this;
	}
	
	/**/
	
	public static Collection<Privilege> getLeaves(Collection<Privilege> privileges) {
		if(CollectionHelper.isEmpty(privileges))
			return null;
		Collection<Privilege> leaves = null;
		for(Privilege privilege : privileges) {
			if(!Boolean.TRUE.equals(isLeaf(privileges, privilege)))
				continue;
			if(leaves == null)
				leaves = new ArrayList<>();
			leaves.add(privilege);
		}
		return leaves;
	}
	
	public static Boolean isLeaf(Collection<Privilege> parents,Privilege child) {
		if(CollectionHelper.isEmpty(parents) || child == null)
			return null;
		return !Boolean.TRUE.equals(hasChild(parents, child));
	}
	
	public static Boolean hasChild(Collection<Privilege> children,Privilege parent) {
		if(CollectionHelper.isEmpty(children) || parent == null)
			return null;
		for(Privilege child : children) {
			if(parent.getIdentifier().equals(child.getIdentifier()))
				continue;
			if(parent.getIdentifier().equals(child.getParentIdentifier()))
				return Boolean.TRUE;
		}
		return Boolean.FALSE;
	}
	
	public static Privilege getParent(Collection<Privilege> parents,Privilege child) {
		if(CollectionHelper.isEmpty(parents) || child == null)
			return null;
		for(Privilege parent : parents) {
			if(parent.getIdentifier().equals(child.getIdentifier()))
				continue;
			if(parent.getIdentifier().equals(child.getParentIdentifier()))
				return parent;
		}
		return null;
	}
	
	/**/
	
	public static final String FIELD_TYPE = "type";
	public static final String FIELD_PARENT_IDENTIFIER = "parentIdentifier";
	
	public static final String COLUMN_TYPE = "type";
	public static final String COLUMN_PARENT_IDENTIFIER = "parent";
	
	public static final String TABLE_NAME = "VM_APP_PRIVILEGE";	
}