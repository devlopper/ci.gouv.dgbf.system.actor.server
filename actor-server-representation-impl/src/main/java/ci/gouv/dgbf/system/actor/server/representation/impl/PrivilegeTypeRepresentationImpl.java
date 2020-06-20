package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.PrivilegeTypeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.PrivilegeTypeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class PrivilegeTypeRepresentationImpl extends AbstractRepresentationEntityImpl<PrivilegeTypeDto> implements PrivilegeTypeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
