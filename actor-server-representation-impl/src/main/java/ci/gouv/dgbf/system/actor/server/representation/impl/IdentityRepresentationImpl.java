package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.IdentityRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.IdentityDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class IdentityRepresentationImpl extends AbstractRepresentationEntityImpl<IdentityDto> implements IdentityRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
