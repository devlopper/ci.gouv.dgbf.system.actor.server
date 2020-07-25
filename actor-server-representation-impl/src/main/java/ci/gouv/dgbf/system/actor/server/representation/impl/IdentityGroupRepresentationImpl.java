package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.IdentityGroupRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.IdentityGroupDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class IdentityGroupRepresentationImpl extends AbstractRepresentationEntityImpl<IdentityGroupDto> implements IdentityGroupRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
